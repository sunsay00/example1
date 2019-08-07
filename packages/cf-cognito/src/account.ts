import CognitoUtil from './cognitoutil';
import UserLogin from './userlogin';
import UserProfile from './userprofile';
import UserRegistration from './userregistration';
import { CognitoUserSession, CognitoClient, CognitoUser, UserPoolMode, AccountCredentials, UserState, Obj, LocalStorage, Storage } from './types';
import { vars } from './vars';
import * as RT from 'runtypes';

const UserAttributesRecord = RT.Record({
  'custom:role': RT.String,
  email: RT.String,
  email_verified: RT.String,
  locale: RT.String,
  preferred_username: RT.String,
  sub: RT.String
});

type UserAttributes = RT.Static<typeof UserAttributesRecord>;

export type Tokens = {
  idToken: string, accessToken: string, refreshToken: string,
  awsSecretAccessKey: string, awsSessionToken: string, awsAccessKeyId: string
};

export type User = {
  username: string,
  sub: string,
  locale: string,
  email: string
  role: string,
  groups: string[],
  tokens: Tokens
}

export class Account {
  private _mode: UserPoolMode;
  private _client: CognitoClient | undefined;
  private _util: CognitoUtil | undefined;
  private _reg: UserRegistration | undefined;
  private _login: UserLogin | undefined;
  private _profile: UserProfile | undefined;
  private _storage: LocalStorage;
  private _initialized: boolean = false;

  constructor(mode: UserPoolMode, storage: Storage) {
    this._mode = mode;
    const prefix = '@account:';
    const getAllKeys = () => new Promise<string[]>((resolve, reject) =>
      storage.getAllKeys((err, keys) => err ? reject(err) : resolve((keys || []).filter(k => k.startsWith(prefix)).map(k => k.substr(prefix.length)))));
    const remove = (key: string) => new Promise<void>((resolve, reject) =>
      storage.removeItem(`${prefix}${key}`, err => err ? reject(err) : resolve()));
    const clear = async () => {
      const keys = await getAllKeys();
      for (let key of keys)
        await remove(key);
    }
    this._storage = {
      getAllKeys, remove, clear,
      set: (key: string, value?: string) => new Promise<void>((resolve, reject) =>
        storage.setItem(`${prefix}${key}`, value || '', err => err ? reject(err) : resolve())),
      get: (key: string) => new Promise<string | undefined>((resolve, reject) =>
        storage.getItem(`${prefix}${key}`, (err, val) => err ? reject(err) : resolve(val === null ? undefined : val))),
      setObject: (key: string, value: object) => new Promise<void>((resolve, reject) =>
        storage.setItem(`${prefix}${key}`, JSON.stringify(value), err => err ? reject(err) : resolve())),
      getObject: (key: string) => new Promise<object | undefined>((resolve, reject) =>
        storage.getItem(`${prefix}${key}`, (err, data) => err ? reject(err) : resolve(data && JSON.parse(data)))),
    };
  }
  init = async (region: string): Promise<boolean> => {
    if (this._initialized) return true;
    const { Client } = this._mode == UserPoolMode.Web ? await import('./web-client') : await import('./rn-client');
    this._client = new Client(region, this._mode);
    this._util = new CognitoUtil(this._client, region, this._mode, this._storage);
    this._reg = new UserRegistration(this._client, this._mode, this._util);
    this._profile = new UserProfile(this._client, this._util, this._storage);
    this._login = new UserLogin(this._client, region, this._profile, this._util, this._storage);
    await this._util.sync();
    this._initialized = true;
    return this._initialized;
  }
  signUp = async (username: string, email: string, password: string, locale: string, role: string): Promise<string> => {
    if (!this._reg) throw new Error('not initialized');
    const ret = await this._reg.signUp(username, email, password, locale, role);
    return ret;
  }
  signIn = async (emailOrUsername: string, password: string): Promise<'success' | 'changepassword'> => {
    if (!this._login) throw new Error('not initialized');
    const ret = await this._login.signIn(emailOrUsername, password);
    //Sentry.setUserContext({
    //email: emailOrUsername,
    //id: await this.sub(),
    //username: emailOrUsername,
    //});
    return ret;
  }
  signOut = async () => {
    if (!this._login) throw new Error('not initialized');
    await this._login.signOut();
    await this._storage.clear();
    //Sentry.setUserContext({ id: sub.ok });
  }
  confirmSignUp = async (confirmationCode: string): Promise<boolean> => {
    if (!this._reg) throw new Error('not initialized');
    return await this._reg.confirmSignUp(confirmationCode);
  }
  resendConfirmationCode = async (): Promise<void> => {
    if (!this._reg) throw new Error('not initialized');
    return await this._reg.resendConfirmationCode();
  }
  forgotPassword = async (username: string): Promise<void> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.forgotPassword(username);
  }
  confirmForgotPassword = async (username: string, verificationCode: string, password: string): Promise<void> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.confirmForgotPassword(username, verificationCode, password);
  }
  changePassword = async (prevPassword: string, newPassword: string): Promise<void> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.changePassword(prevPassword, newPassword);
  }
  completeNewPasswordChallenge = async (newPassword: string, locale: string): Promise<any> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.completeNewPasswordChallenge(newPassword, locale);
  }
  refreshCredentials = async (): Promise<void> => {
    if (!this._client || !this._util || !this._profile || !this._login) throw new Error('not initialized');
    const client = this._client;
    const util = this._util;
    const profile = this._profile;
    const login = this._login;
    // Workaround: aws cognito user device tracking is not used by this application. however react-native-aws-cognito-js@0.0.4
    // does not expose an easy way to disable this. by surveying its sourcecode, clearing out the LastAuthUser value from
    // the storage used by the cognito-user object will fix.
    const user = await this._util.getCurrentUser();
    if (!user) throw new Error('invalid cognito user');
    const clientId = this._mode == UserPoolMode.Web ? vars.WebUserPoolClientId : vars.MobileUserPoolClientId;
    const lastUserKeyKey = `CognitoIdentityServiceProvider.${clientId}.LastAuthUser`;
    const lastUserKey = (user as any).storage.getItem(lastUserKeyKey);
    if (lastUserKey)
      (user as any).storage.removeItem(lastUserKeyKey);

    return await new Promise<void>((resolve, reject) => {
      user.getSession((err: Error, session: CognitoUserSession) => {
        if (err) { return reject(err); }
        const refreshToken = session.getRefreshToken();
        user.refreshSession(refreshToken, async (err: Error, session: CognitoUserSession) => {
          if (err) { return reject(err); }
          try {
            const newAccessToken = await session.getAccessToken().getJwtToken();
            const newIdToken = await session.getIdToken().getJwtToken();
            const newRefreshToken = await session.getRefreshToken().getToken();
            await this._storage.set('userTokens.accessToken', newAccessToken);
            await this._storage.set('userTokens.idToken', newIdToken);
            await this._storage.set('userTokens.refreshToken', newRefreshToken);
            await profile.getUserAttributes();
            await login.getAwsCredentials();
            await this._storage.set('userId', util.getCognitoIdentityId());
            await this._storage.set('userTokens.awsAccessKeyId', client.accessKeyId());
            await this._storage.set('userTokens.awsSecretAccessKey', client.secretAccessKey());
            await this._storage.set('userTokens.awsSessionToken', client.sessionToken());
            resolve();
          } catch (err) {
            reject(err);
          }
        });
      });
    });
  }
  private tokens = async (): Promise<{
    idToken: string, accessToken: string, refreshToken: string,
    awsSecretAccessKey: string, awsSessionToken: string, awsAccessKeyId: string
  } | undefined> => {
    const accessToken = await this._storage.get('userTokens.accessToken');
    const idToken = await this._storage.get('userTokens.idToken');
    const refreshToken = await this._storage.get('userTokens.refreshToken');
    const awsAccessKeyId = await this._storage.get('userTokens.awsAccessKeyId');
    const awsSecretAccessKey = await this._storage.get('userTokens.awsSecretAccessKey');
    const awsSessionToken = await this._storage.get('userTokens.awsSessionToken');
    if (accessToken && idToken && refreshToken && awsAccessKeyId && awsSecretAccessKey && awsSessionToken)
      return { accessToken, idToken, refreshToken, awsAccessKeyId, awsSecretAccessKey, awsSessionToken };
    else
      return undefined;
  }

  getCurrentUser = async (): Promise<User | undefined> => {
    if (!this._util || !this._profile) throw new Error('not initialized');
    const accessToken = await this._storage.get('userTokens.accessToken');
    if (!accessToken) return undefined;
    const user = await this._util.getCurrentUser();
    if (!user) return undefined;
    const groups = await this._util.getUserGroup();
    const attrs = await this._profile.getUserAttributes();
    if (!UserAttributesRecord.guard(attrs)) {
      console.error('invalid user attributes');
      await this.signOut();
      return undefined;
    } else {
      const tokens = await this.tokens();
      if (!tokens) {
        console.error('invalid account tokens');
        await this.signOut();
        return undefined;
      } else {
        return {
          username: user.getUsername(),
          sub: attrs.sub,
          locale: attrs.locale,
          email: attrs.email,
          role: attrs["custom:role"],
          groups,
          tokens
        };
      }
    }
  }

}
