import * as AWS from 'aws-sdk';
import { AuthenticationDetails as AWSAuthenticationDetails, CognitoUserSession as AWSCognitoUserSession, ICognitoUser, CognitoUserPool as AWSCognitoUserPool, CognitoUser as AWSCognitoUser, CognitoUserAttribute } from 'amazon-cognito-identity-js';
import { outputs as vars } from './vars';
import { AuthenticationDetails, CognitoUserSession, CognitoIdentityServiceProvider, CognitoUser, CognitoClient, UserPoolMode, UserPool } from './types';

//export type CognitoUserSession = AWSCognitoUserSession;
//export type CognitoUserAttribute = CognitoUserAttribute;
//export type ICognitoUserPool = ICognitoUserPool;
//export type ICognitoUser = ICognitoUser;

class UserAdaptor implements CognitoUser {
  _user: AWSCognitoUser;
  constructor(user: AWSCognitoUser) {
    console.assert(user != undefined);
    this._user = user;
  }
  getUsername = this._user.getUsername;
  confirmRegistration = this._user.confirmRegistration;
  getSession = this._user.getSession;
  updateAttributes = this._user.updateAttributes;
  getAttributeVerificationCode = this._user.getAttributeVerificationCode;
  verifyAttribute = this._user.verifyAttribute;
  getUserAttributes = this._user.getUserAttributes;
  signOut = this._user.signOut;
  changePassword = this._user.changePassword;
  globalSignOut = this._user.globalSignOut;
  forgotPassword = this._user.forgotPassword;
  confirmPassword = this._user.confirmPassword;
  refreshSession = this._user.refreshSession;
  completeNewPasswordChallenge = this._user.completeNewPasswordChallenge
  authenticateUser = (details: AuthenticationDetails, callbacks: {
    newPasswordRequired: (userAttributes: any, requiredAttributes: any) => void,
    customChallenge: (challengeParameters: any) => void,
    mfaRequired: (challengeName: any, challengeParameter: any) => void,
    onSuccess: (session: CognitoUserSession) => void,
    onFailure: (err: any) => void
  }) => {
    this._user.authenticateUser(details, {
      newPasswordRequired: callbacks.newPasswordRequired,
      customChallenge: callbacks.customChallenge,
      mfaRequired: callbacks.mfaRequired,
      onFailure: callbacks.onFailure,
      onSuccess: (session: AWSCognitoUserSession) => {
        callbacks.onSuccess({
          getAccessToken: () => ({ getJwtToken: async () => session.getAccessToken().getJwtToken() }),
          getIdToken: () => ({ getJwtToken: async () => session.getIdToken().getJwtToken() }),
          getRefreshToken: () => ({ getToken: async () => session.getRefreshToken().getToken() }),
        });
      },
    });
  }
}

class UserPoolAdaptor implements UserPool {
  _pool: AWSCognitoUserPool;
  _user: UserAdaptor;
  constructor(pool: AWSCognitoUserPool) {
    this._pool = pool;
    this._user = new UserAdaptor(pool.getCurrentUser());
  }
  signUp = this._pool.signUp;
  getCurrentUser = () => this._user
};

export class Client implements CognitoClient {
  private _mode: UserPoolMode;
  constructor(region: string, mode: UserPoolMode) {
    AWS.config.update({ region });
    this._mode = mode;
  }

  refreshCredentials = (accessKeyId: string, secretAccessKey: string, sessionToken: string) => {
    AWS.config.accessKeyId = accessKeyId;
    AWS.config.secretAccessKey = secretAccessKey;
    AWS.config.sessionToken = sessionToken;
  }

  setCognitoIdentityPoolDetails = (Logins?: { [_: string]: string }): CognitoIdentityCredentials => {
    // Set Cognito Identity Pool details
    const params: AWS.CognitoIdentityCredentials.CognitoIdentityOptions = {
      IdentityPoolId: vars.CognitoIdentityPoolId,
      Logins
    };
    const credentials = new AWS.CognitoIdentityCredentials(params);
    AWS.config.credentials = credentials;
    return credentials;
  }

  private _createCognitoUserPool = (): AWSCognitoUserPool => {
    // Initialize Cognito User Pool
    const poolData = {
      UserPoolId: vars.UserPoolId,
      ClientId: this._mode == UserPoolMode.Web ? vars.WebUserPoolClientId : vars.MobileUserPoolClientId,
    };

    //// Initialize AWS config object with dummy keys - required if unauthenticated access is not enabled for identity pool
    //AWSCognito.config.update({accessKeyId: 'dummyvalue', secretAccessKey: 'dummyvalue'});
    return new AWSCognitoUserPool(poolData);
  }
  createCognitoUserPool = (): UserPool => new UserPoolAdaptor(this._createCognitoUserPool());

  createAuthenticationDetails = (Username: string, Password: string) => {
    const authenticationData = { Username, Password };
    return new AWSAuthenticationDetails(authenticationData);
  }

  createCognitoUser = (username: string): CognitoUser => {
    this.setCognitoIdentityPoolDetails();
    const userData: ICognitoUser = {
      Username: username,
      Pool: this._createCognitoUserPool(),
    };
    return new UserAdaptor(new AWSCognitoUser(userData));
  }

  cognitoIdentityId = (): string => {
    const credentials = AWS.config.credentials;
    if (!(credentials instanceof AWS.CognitoIdentityCredentials)) {
      throw new Error('failed to get cognito identity id');
    }
    return credentials.identityId;
  }

  currentUser = () => {
    this.setCognitoIdentityPoolDetails();
    return this.createCognitoUserPool().getCurrentUser();
  }

  accessKeyId = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.accessKeyId;
  }

  secretAccessKey = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.secretAccessKey;
  }

  sessionToken = (): string | undefined => {
    if (!AWS.config.credentials) return undefined;
    return AWS.config.credentials.sessionToken || undefined;
  }

  clearCachedId = () => {
    const credentials = AWS.config.credentials;
    if (credentials instanceof AWS.CognitoIdentityCredentials) {
      credentials.clearCachedId();
    }
  }

  createCognitoUserAttribute = (Name: string, Value: string) => {
    return new CognitoUserAttribute({ Name, Value });
  }

  createCognitoServiceProvider = (): CognitoIdentityServiceProvider => {
    return new AWS.CognitoIdentityServiceProvider();
  }
};