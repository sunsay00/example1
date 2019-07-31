import { AuthenticationDetails, CognitoUserPool, CognitoUser, CognitoUserAttribute } from 'react-native-aws-cognito-js';
import { Config as AWSConfig, CognitoIdentityCredentials, CognitoIdentityServiceProvider } from 'aws-sdk/dist/aws-sdk-react-native';
import { Obj, UserPoolMode, AWSCognitoUserAttribute, AWSCognitoUserSession, UserPool } from './types';
import { outputs } from './vars';

export type CognitoUserSession = AWSCognitoUserSession;
export type CognitoUserAttribute = AWSCognitoUserAttribute;

export default class AWSClient {

  static refreshCredentials = (accessKeyId: string, secretAccessKey: string, sessionToken: string) => {
    AWSConfig.credentials.accessKeyId = accessKeyId;
    AWSConfig.credentials.secretAccessKey = secretAccessKey;
    AWSConfig.credentials.sessionToken = sessionToken;
  }

  static setCognitoIdentityPoolDetails = (region: string, logins?: Obj<string>): CognitoIdentityCredentials => {
    // Set Cognito Identity Pool details
    AWSConfig.region = region;
    const params = logins === undefined ? {
      IdentityPoolId: outputs.CognitoIdentityPoolId,
    } : {
        IdentityPoolId: outputs.CognitoIdentityPoolId,
        Logins: logins,
      };
    const credentials = new CognitoIdentityCredentials(params);
    AWSConfig.credentials = credentials;
    return credentials;
  }
  static createCognitoUserPool = (mode: UserPoolMode): UserPool => {
    // Initialize Cognito User Pool
    const poolData = {
      UserPoolId: outputs.UserPoolId,
      ClientId: mode == UserPoolMode.Web ? outputs.WebUserPoolClientId : outputs.MobileUserPoolClientId
    };

    //// Initialize AWS config object with dummy keys - required if unauthenticated access is not enabled for identity pool
    //AWSCognito.config.update({accessKeyId: 'dummyvalue', secretAccessKey: 'dummyvalue'});
    return new CognitoUserPool(poolData);
  }

  static createAuthenticationDetails = (Username: string, Password: string): AuthenticationDetails => {
    const authenticationData = { Username, Password };
    return new AuthenticationDetails(authenticationData);
  }

  static createCognitoUser = (region: string, mode: UserPoolMode, username: string) => {
    AWSClient.setCognitoIdentityPoolDetails(region);
    return new CognitoUser({
      Username: username,
      Pool: AWSClient.createCognitoUserPool(mode),
    });
  }

  static cognitoIdentityId = (): string => {
    const credentials = AWSConfig.credentials;
    if (!(credentials instanceof CognitoIdentityCredentials)) {
      throw new Error('failed to get cognito identity id');
    }
    return credentials.identityId;
  }

  static currentUser = (region: string, mode: UserPoolMode) => {
    AWSClient.setCognitoIdentityPoolDetails(region);
    const user = AWSClient.createCognitoUserPool(mode).getCurrentUser();
    if (user == null) {
      throw new Error('current cognito user is null, perhaps there was a mix up of environment state? (eg. ClientId)');
    }
    return user;
  }

  static accessKeyId = (): string | undefined => {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.accessKeyId;
  }

  static secretAccessKey = (): string | undefined => {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.secretAccessKey;
  }

  static sessionToken(): string | undefined {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.sessionToken || undefined;
  }

  static clearCachedId() {
    const credentials = AWSConfig.credentials;
    if (credentials instanceof CognitoIdentityCredentials) {
      credentials.clearCachedId();
    }
  }

  static createCognitoUserAttribute(Name: string, Value: string) {
    return new CognitoUserAttribute({ Name, Value });
  }

  static createCognitoServiceProvider = () => {
    return new CognitoIdentityServiceProvider();
  }
};

