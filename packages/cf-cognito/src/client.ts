import * as AWS from 'aws-sdk';
import { ICognitoStorage, AuthenticationDetails as AWSAuthenticationDetails, CognitoUserPool as AWSCognitoUserPool, CognitoUser as AWSCognitoUser, CognitoUserAttribute } from 'amazon-cognito-identity-js';
import { vars } from './vars';
import { CompleteNewPasswordChallengeHandler, AuthenticationDetails, CognitoUserSession, CognitoIdentityServiceProvider, CognitoUser, CognitoClient, UserPoolMode, UserPool, LocalStorage } from './types';

class StorageAdaptor {
  static MEMORY_KEY_PREFIX = '@cognito:';
  prototype: any;
  private _storage: LocalStorage;
  private _mem: { [_: string]: string | undefined } = {};
  constructor(storage: LocalStorage) {
    this._storage = storage;
  }
  getStorage = (): ICognitoStorage => ({
    setItem: (key: string, value: string): string => {
      if (value) {
        this._storage.set(`${StorageAdaptor.MEMORY_KEY_PREFIX}${key}`, value);
        this._mem[key] = value;
      } else {
        this._storage.remove(`${StorageAdaptor.MEMORY_KEY_PREFIX}${key}`);
        delete this._mem[key];
      }
      return value;
    },
    getItem: (key: string): string => this._mem[key] || '',
    removeItem: (key: string): boolean => {
      this._storage.remove(`${StorageAdaptor.MEMORY_KEY_PREFIX}${key}`);
      return delete this._mem[key];
    },
    clear: () => {
      this._mem = {};
      return this._mem;
    },
  })
  sync = async (callback: (err: Error | null, ret: 'SUCCESS' | null) => void) => {
    try {
      const keys = await this._storage.getAllKeys();
      const memkeys = keys.filter(key => key.startsWith(StorageAdaptor.MEMORY_KEY_PREFIX));
      const mem: { [_: string]: string | undefined } = {};
      for (let k of memkeys)
        mem[k.replace(StorageAdaptor.MEMORY_KEY_PREFIX, '')] = await this._storage.get(k);
      this._mem = mem;
      callback(null, 'SUCCESS');
    } catch (err) {
      callback(err, null);
    }
  }
}

class UserAdaptor implements CognitoUser {
  private _user: AWSCognitoUser;
  constructor(user: AWSCognitoUser) {
    this._user = user;
  }
  getUsername = () => this._user.getUsername();
  confirmRegistration = (confirmationCode: string, b: boolean, next: (err: Error, data: any) => void) => this._user.confirmRegistration(confirmationCode, b, next);
  getSession = (next: (err: Error, session: any) => void) => this._user.getSession(next);
  updateAttributes = (attributeList: any[], next: (err?: Error, result?: string) => void) => this._user.updateAttributes(attributeList, next);
  getAttributeVerificationCode = (name: string, params: {
    onSuccess: () => void,
    onFailure: (err: Error) => void,
    inputVerificationCode?: (data: string) => void | null,
  }) => this._user.getAttributeVerificationCode(name, params);
  verifyAttribute = (attribute: any, verificationCode: string, params: { onFailure: (err: any) => void, onSuccess: (data: string) => void }) => this._user.verifyAttribute(attribute, verificationCode, params);
  getUserAttributes = (next: (err?: Error, result?: CognitoUserAttribute[]) => void) => this._user.getUserAttributes(next);
  signOut = () => this._user.signOut();
  changePassword = (previousPassword: string, proposedPassword: string, next: (err?: Error, result?: 'SUCCESS') => void) => this._user.changePassword(previousPassword, proposedPassword, next);
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
  private _user: UserAdaptor | null;
  constructor(pool: AWSCognitoUserPool, user: UserAdaptor | null) {
    this._pool = pool;
    this._user = user;
  }
  getUserPoolId = () => this._pool.getUserPoolId()
  getClientId = () => this._pool.getClientId()
  signUp = (username: string, password: string, attributeList: any[], validationData: any[], fn: (err: any, result: any) => void) => this._pool.signUp(username, password, attributeList, validationData, fn);
  getCurrentUser = () => this._user
};

export class Client implements CognitoClient {
  private _storage: StorageAdaptor;
  private _mode: UserPoolMode;
  constructor(region: string, mode: UserPoolMode, storage: LocalStorage) {
    AWS.config.update({ region });
    this._mode = mode;
    this._storage = new StorageAdaptor(storage);
  }

  init = async () => new Promise<void>((resolve, reject) => {
    this._storage.sync((err, _ret) => {
      if (err) reject(err);
      else resolve();
    });
  })

  refreshCredentials = (accessKeyId: string, secretAccessKey: string, sessionToken: string) => {
    AWS.config.accessKeyId = accessKeyId;
    AWS.config.secretAccessKey = secretAccessKey;
    AWS.config.sessionToken = sessionToken;
  }

  setCognitoIdentityPoolDetails = (Logins?: { [_: string]: string }): CognitoIdentityCredentials => {
    const credentials = new AWS.CognitoIdentityCredentials({
      IdentityPoolId: vars.CognitoIdentityPoolId,
      Logins
    });
    AWS.config.credentials = credentials;
    return credentials;
  }

  private _createCognitoUserPool = (): AWSCognitoUserPool => {
    // Initialize Cognito User Pool
    return new AWSCognitoUserPool({
      UserPoolId: vars.UserPoolId,
      ClientId: this._mode == UserPoolMode.Web ? vars.WebUserPoolClientId : vars.MobileUserPoolClientId,
      Storage: this._storage.getStorage(),
    });
  }
  createCognitoUserPool = (): UserPool => {
    const pool = this._createCognitoUserPool();
    const user = pool.getCurrentUser();
    return new UserPoolAdaptor(pool, user && new UserAdaptor(user) || null);
  }

  createAuthenticationDetails = (Username: string, Password: string) => {
    return new AWSAuthenticationDetails({
      Username,
      Password,
    });
  }

  createCognitoUser = (username: string): CognitoUser => {
    this.setCognitoIdentityPoolDetails();
    const user = new AWSCognitoUser({
      Username: username,
      Pool: this._createCognitoUserPool(),
      Storage: this._storage.getStorage(),
    });
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