import { GraphQLError, ExecutionResult } from 'graphql';
import { domainWrapper } from './domainwrapper';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';

export type UserContext = {
  sub: string,
  groups?: string[],
  username?: string,
};

export const apiWrapper = (params: {
  stage: string,
  corsAllowOrigin: string,
}) => (onMain: (
  headers: { [_: string]: string },
  query: string,
  variables: { [_: string]: string },
  user: UserContext) => Promise<{ errors?: readonly GraphQLError[], data?: ExecutionResult<unknown> }>
) => domainWrapper(async (event: APIGatewayProxyEvent, context: Context) => {

  context.callbackWaitsForEmptyEventLoop = false; // let's not wait around for event loop to empty out

  const headers: { [_: string]: string } = {};
  if (event.headers) {
    // collect headers and lowercase them
    Object.keys(event.headers).forEach(key =>
      headers[key.toLowerCase()] = event.headers[key]
    );

    // only allow json
    if (headers['content-type'].indexOf('application/json') != 0)
      throw new Error(`expected json content-type, received '${headers['content-type']}'`);
  }

  // useful for testing 401 reponses
  if (headers['user-agent'] && headers['user-agent'].indexOf('-401testing-') !== -1) {
    return {
      statusCode: 401,
      headers: {
        'Access-Control-Allow-Origin': params.corsAllowOrigin, // Required for CORS support to work
      },
      body: JSON.stringify({ message: '401-Testing' }),
    };
  }

  // parse context payload
  let payload = null;
  if (event.requestContext && event.requestContext.authorizer) {
    try {
      payload = JSON.parse(event.requestContext.authorizer.payload);
      //payload = JSON.parse(event.requestContext.authorizer.payload);
    } catch (e) {
      throw new Error('failed to parse payload');
    }
  } else {
    throw new Error('failed to parse payload (2)');
  }

  if (payload.unauthorized) {
    // a long outstanding AWS error (https://forums.aws.amazon.com/thread.jspa?messageID=728839) prevents
    // CORS headers from being passed upon non successfull responses, as a workaround use an
    // unauthorized-policy so proper reponses and be sent from the api handler
    return {
      statusCode: payload.statusCode,
      headers: {
        'Access-Control-Allow-Origin': params.corsAllowOrigin, // Required for CORS support to work
      },
      body: JSON.stringify({ message: payload.message }),
    };
  }

  // `event.body` should be an unparsed JSON string, however serverless-offline
  // pre-parses it for us so we need to make an exception for it
  let json = null;
  if (typeof event.body !== 'string') {
    if (process.env.NODE_ENV !== 'test' && process.env.NODE_ENV !== 'local') {
      throw new Error(`event body should be a string, got ${event.body}`);
    } else {
      json = event.body;
    }
  } else {
    try {
      json = JSON.parse(event.body);
    } catch (e) {
      throw new Error(`failed to parse body: ${event.body}`);
    }
  }

  if (!json.query)
    throw new Error('neither query nor variables not defined in the request');

  const user: UserContext = {
    sub: payload.sub,
    groups: payload['cognito:groups'],
    username: payload.username,
  };

  if (process.env.NODE_ENV === 'local') {
    console.log('[GRAPHQL-REQUEST]');
    console.log('  QUERY:', json.query);
    console.log('  VARIABLES:', json.variables);
    console.log('  USER:', user);
    //console.log('   HEADERS:', headers);
  }

  try {
    // get requested api version from query string, seems to be sent in two locations, check both
    //const v = event.queryStringParameters != undefined ? event.queryStringParameters.v : (event.query != undefined ? event.query.v : '');
    //if (!v) throw new Error('v not specified');

    const result = await onMain(headers, json.query, json.variables, user);

    // unhandled exceptions that occur in client graphql do not
    // propagate to the catch handler, instead are handled here
    if (result.errors && result.errors.length > 0 && result.errors[0] instanceof Error) {
      throw result.errors[0];
    }
    // discripency between serverless-offline and actual aws lambda,
    // need to return different results
    if (process.env.NODE_ENV === 'local') {
      return { data: result.data };
    } else {
      return {
        statusCode: 200,
        headers: {
          'Access-Control-Allow-Origin': params.corsAllowOrigin, // Required for CORS support to work
        },
        body: JSON.stringify(result),
      };
    }
  } catch (err) {
    if (params.stage != 'local' && params.stage != 'production')
      console.log(err.stack);
    return {
      statusCode: 400,
      headers: {
        'Access-Control-Allow-Origin': params.corsAllowOrigin, // Required for CORS support to work
      },
      body: JSON.stringify({ message: err.message }),
    };
  }
})
