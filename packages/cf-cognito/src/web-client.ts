/*
import * as AWS from 'aws-sdk';
import { AuthenticationDetails, CognitoUserSession, ICognitoUserPool, ICognitoUser, CognitoUserPool, CognitoUser, CognitoUserAttribute } from 'amazon-cognito-identity-js';

export const init = async () => {
  AWS.config.update({ region: config('AWS_REGION') });
};

export type CognitoUserSession = CognitoUserSession;
export type CognitoUserAttribute = CognitoUserAttribute;
export type ICognitoUserPool = ICognitoUserPool;
export type ICognitoUser = ICognitoUser;

export default class AWSClient {
  constructor(region: string) {

  }

  public static AWS_REGION = () => config('AWS_REGION');
  public static AWS_COGNITO_USER_POOL_ID = () => config('AWS_COGNITO_USER_POOL_ID');
  public static AWS_COGNITO_APP_CLIENT_ID = () => config('AWS_COGNITO_APP_CLIENT_ID');
  public static AWS_COGNITO_IDENTITY_POOL_ID = () => config('AWS_COGNITO_IDENTITY_POOL_ID');

  public static setCognitoIdentityPoolDetails = (logins?: Hash<string>) => {
    // Set Cognito Identity Pool details
    AWS.config.region = config('AWS_REGION');
    const params = logins === undefined ? {
      IdentityPoolId: config('AWS_COGNITO_IDENTITY_POOL_ID'),
    } : {
        IdentityPoolId: config('AWS_COGNITO_IDENTITY_POOL_ID'),
        Logins: logins,
      };
    const credentials = new AWS.CognitoIdentityCredentials(params);
    AWS.config.credentials = credentials;
    return credentials;
  }
  public static createCognitoUserPool = () => {
    // Initialize Cognito User Pool
    const poolData = {
      UserPoolId: config('AWS_COGNITO_USER_POOL_ID'),
      ClientId: config('AWS_COGNITO_APP_CLIENT_ID'),
    };

    //// Initialize AWS config object with dummy keys - required if unauthenticated access is not enabled for identity pool
    //AWSCognito.config.update({accessKeyId: 'dummyvalue', secretAccessKey: 'dummyvalue'});
    return new CognitoUserPool(poolData);
  }

  public static createAuthenticationDetails = (Username: string, Password: string) => {
    const authenticationData = { Username, Password };
    return new AuthenticationDetails(authenticationData);
  }

  public static createCognitoUser = (username: string) => {
    AWSClient.setCognitoIdentityPoolDetails();
    const userData: ICognitoUser = {
      Username: username,
      Pool: AWSClient.createCognitoUserPool(),
    };
    return new CognitoUser(userData);
  }

  public static cognitoIdentityId = (): string => {
    const credentials = AWS.config.credentials;
    if (!(credentials instanceof AWS.CognitoIdentityCredentials)) {
      throw new Error('failed to get cognito identity id');
    }
    return credentials.identityId;
  }

  public static currentUser = () => {
    AWSClient.setCognitoIdentityPoolDetails();
    return AWSClient.createCognitoUserPool().getCurrentUser();
  }

  public static accessKeyId = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.accessKeyId;
  }

  public static secretAccessKey = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.secretAccessKey;
  }

  public static sessionToken = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.sessionToken || undefined;
  }

  public static clearCachedId = () => {
    const credentials = AWS.config.credentials;
    if (credentials instanceof AWS.CognitoIdentityCredentials) {
      credentials.clearCachedId();
    }
  }

  public static createCognitoUserAttribute = (Name: string, Value: string) => {
    return new CognitoUserAttribute({ Name, Value });
  }

  public static createCognitoServiceProvider = () => {
    return new AWS.CognitoIdentityServiceProvider();
  }
};
*/