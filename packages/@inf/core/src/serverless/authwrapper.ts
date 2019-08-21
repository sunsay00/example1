import { domainWrapper } from './domainwrapper';
import * as jwt from 'jsonwebtoken';
import { CustomAuthorizerEvent, Context } from 'aws-lambda';
import fetch from 'node-fetch';
const jwkToPem = require('jwk-to-pem');

type PemKey = { kid: string, n: string, e: string, kty: string };
type Statement = { Action: string, Effect: string, Resource: string };
type AuthResponse = { principalId: string, policyDocument: PolicyDocument, context: { [key: string]: any } };
type PolicyDocument = { Version: string, Statement: Statement[] };
type ApiOptions = { region: string, restApiId: string, stage: string };
//type Event = { methodArn: string, authorizationToken: string };

let PEMS: { [key: string]: string } | null = null;

const generatePolicy = (principalId: string, resource: string, payload: object) => {
  const authResponse = {} as AuthResponse;
  authResponse.principalId = principalId;
  if (resource) {
    const policyDocument = {} as PolicyDocument;
    policyDocument.Version = '2012-10-17';
    policyDocument.Statement = [];
    const statementOne = {} as Statement;
    statementOne.Action = 'execute-api:Invoke';
    statementOne.Effect = 'Allow';
    statementOne.Resource = resource;
    policyDocument.Statement[0] = statementOne;
    authResponse.policyDocument = policyDocument;
  }

  // context object properties can only be strings, numbers, and booleans
  // http://docs.aws.amazon.com/apigateway/latest/developerguide/use-custom-authorizer.html#api-gateway-custom-authorizer-output
  authResponse.context = { payload: JSON.stringify(payload) };
  return authResponse;
};

// a long outstanding AWS error (https://forums.aws.amazon.com/thread.jspa?messageID=728839) prevents
// CORS headers from being passed upon non successfull responses, as a workaround use an
// unauthorized-policy so proper reponses can be sent from the actual handler
const generateUnauthorizedPolicy = (resource: string, statusCode: number, message: string) => {
  return generatePolicy('', resource, { unauthorized: true, statusCode, message });
};

const generateGuestPolicy = (resource: string) => {
  return generatePolicy('guest', resource, { 'cognito:groups': undefined });
};

const sendPolicy = (event: CustomAuthorizerEvent, payload: { sub: string, ['cognito:groups']: string[] }, apiOptions: ApiOptions, next: (err: any, result: any) => void) => {
  payload['cognito:groups'] = payload['cognito:groups'] || [];
  next(null, generatePolicy(payload.sub, event.methodArn, payload));
};

const processAuthRequest = (nodeEnv: string, event: CustomAuthorizerEvent, tokenIssuer: string, awsAccountId: string, apiOptions: ApiOptions, next: (err: any, result?: object) => void) => {
  if (!event.authorizationToken) {
    console.log('missing authorization token');
    next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    return;
  }

  const matches = /[Bb]earer( EXP)? ([a-zA-Z0-9-._]+)/.exec(event.authorizationToken);
  if (matches == undefined || matches.length != 3) {
    console.log('invalid authorization token');
    next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    return;
  }
  const hasExpiredTag = matches[1] != undefined;
  const token = matches[2];

  // fail if the token is not jwt
  var decodedJwt = jwt.decode(token, { complete: true });
  if (!decodedJwt || typeof decodedJwt == 'string') {
    console.log('failed to decode jwt');
    next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    return;
  }

  // fail if token is not from your User Pool
  if (decodedJwt.payload['iss'] != tokenIssuer) {
    console.log('provided token not from userpool, expected:', tokenIssuer, 'got:', decodedJwt.payload['iss'], `NODE_ENV=${nodeEnv}`);
    next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    return;
  }
  // reject the jwt if it's not an 'Identity Token'
  if (decodedJwt.payload['token_use'] !== 'id') {
    console.log('provided token is not and identity token');
    next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    return;
  }

  // verify the signature of the JWT token to ensure it's really coming from your user pool
  if (nodeEnv === 'local') {
    const payload = jwt.decode(token);
    if (!payload || typeof payload == 'string') {
      console.log('failed to decode token');
      next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
      return;
    }
    sendPolicy(event, payload as any, apiOptions, next);
  } else {
    // get the kid from the token and retrieve corresponding PEM
    const kid = nodeEnv === 'test' ? '' : (decodedJwt.header.kid || '');
    if (PEMS === null) {
      console.log('PEM not found');
      next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
      return;
    }
    const pem = PEMS[kid];
    if (PEMS && !pem) {
      console.log(`invalid identity token for '${kid}'`);
      next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
      return;
    }

    jwt.verify(token, pem, { issuer: tokenIssuer }, (err: unknown, payload: any) => {
      if (err) {
        console.log('error while trying to verify the token', err);
        next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
        return;
      } else if (hasExpiredTag) {
        console.log('hasExpiredTag set - jwt verify forced failure');
        next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
        return;
      }

      sendPolicy(event, payload, apiOptions, next);
    });
  }
};

const toPem = (keyDictionary: PemKey): string => {
  const modulus = keyDictionary.n;
  const exponent = keyDictionary.e;
  const key_type = keyDictionary.kty;
  const jwk = { kty: key_type, n: modulus, e: exponent };
  const pem = jwkToPem(jwk);
  return pem;
}

export const authWrapper = (params: {
  awsRegion: string,
  nodeEnv: string,
  userPoolId: string,
}) => domainWrapper((event: CustomAuthorizerEvent, context: Context) =>
  new Promise<any>(async (resolve, reject) => {
    const next = (err: unknown, result: unknown) => err ? reject(err) : resolve(result);
    try {
      if (!event) { throw new Error('invalid lambda event object'); }
      if (!event.authorizationToken) { throw new Error('missing authorization token'); }

      const token = event.authorizationToken;
      if (token.toLowerCase().startsWith('bearer ')) {
        const result = /^arn:aws:execute-api:([a-z0-9-]+):([a-zA-Z0-9-<> ]+):([a-zA-Z0-9-<> ]+)\/([a-zA-Z0-9]+)\//.exec(event.methodArn);
        if (!result || result.length !== 5) { throw new Error(`invalid methodArn '${event.methodArn}'`); }

        const awsAccountId = result[2];
        const apiOptions: ApiOptions = {
          region: result[1],
          restApiId: result[3],
          stage: result[4],
        };

        const userPoolURI = `https://cognito-idp.${params.awsRegion}.amazonaws.com/${params.userPoolId}`;
        if (PEMS === null) {
          if (params.nodeEnv == 'local') {
            processAuthRequest(params.nodeEnv, event, userPoolURI, awsAccountId, apiOptions, next);
          } else if (params.nodeEnv === 'test') {
            const fs = require('fs');
            const path = require('path');
            const cert = fs.readFileSync(path.join(__dirname, 'test_public.key.pem'));
            if (!cert) throw new Error('failed to load test_public.key.pem' + params.nodeEnv);
            PEMS = { '': cert };
            processAuthRequest(params.nodeEnv, event, userPoolURI, awsAccountId, apiOptions, next);
          } else {
            try {
              //const path = require('path');
              const jwtKeySetURI = `${userPoolURI}/.well-known/jwks.json`;
              const res = await fetch(jwtKeySetURI, {
                headers: { 'content-type': 'application/json' },
                method: 'GET'
              });
              const data = await res.json();
              PEMS = data['keys'].reduce((r: { [key: string]: string }, key: PemKey) => ({ ...r, [`${key.kid}`]: toPem(key) }), {});
              processAuthRequest(params.nodeEnv, event, userPoolURI, awsAccountId, apiOptions, next);
            } catch (err) {
              console.log(`failed to retrieve the keys from the well known user-pool URI - ${err}`);
              next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
            }
          }
        } else {
          processAuthRequest(params.nodeEnv, event, userPoolURI, awsAccountId, apiOptions, next);
        }
      } else if (token.toLowerCase() === 'guest') {
        next(null, generateGuestPolicy(event.methodArn));
      } else {
        next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
      }
    } catch (err) {
      console.log('auth service - unhandler error', err);
      next(null, generateUnauthorizedPolicy(event.methodArn, 401, 'Unauthorized'));
    }
  }));