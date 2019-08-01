import { AuthenticationDetails as AuthDetails, CognitoUserPool, CognitoUser, CognitoUserAttribute } from 'react-native-aws-cognito-js';
import { Config as AWSConfig, CognitoIdentityCredentials, CognitoIdentityServiceProvider } from 'aws-sdk/dist/aws-sdk-react-native';
import { AuthenticationDetails, Obj, UserPoolMode, UserPool, CognitoClient } from './types';
import { vars } from './vars';

export class Client implements CognitoClient {
  private _mode: UserPoolMode;
  constructor(region: string, mode: UserPoolMode) {
    AWSConfig.region = region;
    this._mode = mode;
  }

  refreshCredentials = (accessKeyId: string, secretAccessKey: string, sessionToken: string) => {
    AWSConfig.credentials.accessKeyId = accessKeyId;
    AWSConfig.credentials.secretAccessKey = secretAccessKey;
    AWSConfig.credentials.sessionToken = sessionToken;
  }

  setCognitoIdentityPoolDetails = (Logins?: { [_: string]: string }): CognitoIdentityCredentials => {
    // Set Cognito Identity Pool details
    const credentials = new CognitoIdentityCredentials({
      IdentityPoolId: vars.CognitoIdentityPoolId,
      Logins
    });
    AWSConfig.credentials = credentials;
    return credentials;
  }
  createCognitoUserPool = (): UserPool => {
    // Initialize Cognito User Pool
    const poolData = {
      UserPoolId: vars.UserPoolId,
      ClientId: this._mode == UserPoolMode.Web ? vars.WebUserPoolClientId : vars.MobileUserPoolClientId
    };

    //// Initialize AWS config object with dummy keys - required if unauthenticated access is not enabled for identity pool
    //AWSCognito.config.update({accessKeyId: 'dummyvalue', secretAccessKey: 'dummyvalue'});
    return new CognitoUserPool(poolData);
  }

  createAuthenticationDetails = (Username: string, Password: string): AuthenticationDetails => {
    const authenticationData = { Username, Password };
    return new AuthDetails(authenticationData) as AuthenticationDetails;
  }

  createCognitoUser = (Username: string) => {
    this.setCognitoIdentityPoolDetails();
    const Pool = this.createCognitoUserPool();
    return new CognitoUser({ Username, Pool });
  }

  cognitoIdentityId = (): string => {
    const credentials = AWSConfig.credentials;
    if (!(credentials instanceof CognitoIdentityCredentials)) {
      throw new Error('failed to get cognito identity id');
    }
    return credentials.identityId;
  }

  currentUser = () => {
    this.setCognitoIdentityPoolDetails();
    const user = this.createCognitoUserPool().getCurrentUser();
    if (user == null) {
      throw new Error('current cognito user is null, perhaps there was a mix up of environment state? (eg. ClientId)');
    }
    return user;
  }

  accessKeyId = (): string | undefined => {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.accessKeyId;
  }

  secretAccessKey = (): string | undefined => {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.secretAccessKey;
  }

  sessionToken(): string | undefined {
    if (!AWSConfig.credentials) return undefined;
    return AWSConfig.credentials.sessionToken || undefined;
  }

  clearCachedId() {
    const credentials = AWSConfig.credentials;
    if (credentials instanceof CognitoIdentityCredentials) {
      credentials.clearCachedId();
    }
  }

  createCognitoUserAttribute(Name: string, Value: string) {
    return new CognitoUserAttribute({ Name, Value });
  }

  createCognitoServiceProvider = () => {
    return new CognitoIdentityServiceProvider();
  }
};

