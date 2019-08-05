import * as AWS from 'aws-sdk';
import { AuthenticationDetails as AWSAuthenticationDetails, ICognitoUser, CognitoUserPool as AWSCognitoUserPool, CognitoUser as AWSCognitoUser, CognitoUserAttribute } from 'amazon-cognito-identity-js';
import { vars } from './vars';
import { CompleteNewPasswordChallengeHandler, AuthenticationDetails, CognitoUserSession, CognitoIdentityServiceProvider, CognitoUser, CognitoClient, UserPoolMode, UserPool } from './types';

class UserAdaptor implements CognitoUser {
  private _user: AWSCognitoUser;
  constructor(user: AWSCognitoUser) {
    this._user = user;
  }
  getUsername = () => this._user.getUsername();
  confirmRegistration = (confirmationCode: string, b: boolean, next: (err: Error, data: any) => void) => this._user.confirmRegistration(confirmationCode, b, next);
  getSession = (next: (err: Error, session: any) => void) => this._user.getSession(next);
  updateAttributes = (attributeList: any[], next: (err: Error, result: string) => void) => this._user.updateAttributes(attributeList, next);
  getAttributeVerificationCode = (username: string, params: { onFailure: (err: any) => void, inputVerificationCode: (data: object) => void }) => this._user.getAttributeVerificationCode(username, params);
  verifyAttribute = (attribute: any, verificationCode: string, params: { onFailure: (err: any) => void, onSuccess: (data: string) => void }) => this._user.verifyAttribute(attribute, verificationCode, params);
  getUserAttributes = (next: (err: Error, result: CognitoUserAttribute[]) => void) => this._user.getUserAttributes(next);
  signOut = () => this._user.signOut();
  changePassword = (previousPassword: string, proposedPassword: string, next: (err: Error, result: any) => void) => this._user.changePassword(previousPassword, proposedPassword, next);
  completeNewPasswordChallenge = (newPassword: string, requiredAttributeData: { [_: string]: string | undefined }, handler: CompleteNewPasswordChallengeHandler) => this._user.completeNewPasswordChallenge(newPassword, requiredAttributeData, handler);
  globalSignOut = (next: { onSuccess: (msg: string) => void, onFailure: (err: Error) => void }) => this._user.globalSignOut(next);
  forgotPassword = (next: { onSuccess: (result: any) => void, onFailure: (err: Error) => void, inputVerificationCode: (result: any) => void }) => this._user.forgotPassword(next);
  confirmPassword = (verificationCode: string, password: string, next: { onSuccess: () => void, onFailure: (err: Error) => void }) => this._user.confirmPassword(verificationCode, password, next);
  refreshSession = (refreshToken: any, next: (err: Error, session: any) => void) => this._user.refreshSession(refreshToken, next);
  authenticateUser = (details: AuthenticationDetails, callbacks: {
    newPasswordRequired: (userAttributes: any, requiredAttributes: any) => void,
    customChallenge: (challengeParameters: any) => void,
    mfaRequired: (challengeName: any, challengeParameter: any) => void,
    onSuccess: (session: CognitoUserSession) => void,
    onFailure: (err: any) => void
  }) => {
    this._user.authenticateUser(details, {
      newPasswordRequired: (a, b) => callbacks.newPasswordRequired(a, b),
      customChallenge: a => callbacks.customChallenge(a),
      mfaRequired: (a, b) => callbacks.mfaRequired(a, b),
      onFailure: a => callbacks.onFailure(a),
      onSuccess: session => {
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
  private _pool: AWSCognitoUserPool;
  private _user: UserAdaptor;
  constructor(pool: AWSCognitoUserPool) {
    this._pool = pool;
    this._user = new UserAdaptor(pool.getCurrentUser());
  }
  signUp = (username: string, password: string, attributeList: any[], validationData: any[], fn: (err: any, result: any) => void) => this._pool.signUp(username, password, attributeList, validationData, fn);
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
  createCognitoUserPool = (): UserPool => {
    return new UserPoolAdaptor(this._createCognitoUserPool());
  }

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
    const user = new AWSCognitoUser(userData);
    const ret = new UserAdaptor(user);
    return ret;
  }

  cognitoIdentityId = (): string => {
    const credentials = AWS.config.credentials;
    if (!(credentials instanceof AWS.CognitoIdentityCredentials))
      throw new Error('failed to get cognito identity id');
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