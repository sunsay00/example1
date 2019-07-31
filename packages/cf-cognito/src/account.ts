import Client, { CognitoUserSession } from './client';
import CognitoUtil from './cognitoutil';
import UserLogin from './userlogin';
import UserProfile from './userprofile';
import UserRegistration from './userregistration';
import { User, UserPoolMode, AccountCredentials, UserState, Obj, LocalStorage, Storage } from './types';
import { outputs } from './vars';

export class Account {
  private _mode: UserPoolMode;
  private _util: CognitoUtil | undefined;
  private _reg: UserRegistration | undefined;
  private _login: UserLogin | undefined;
  private _profile: UserProfile | undefined;
  private _storage: LocalStorage;
  private _initialized: boolean = false;

  constructor(mode: UserPoolMode, storage: Storage) {
    this._mode = mode;
    this._storage = {
      clear: () => new Promise<void>((resolve, reject) => storage.clear(err => err ? reject(err) : resolve())),
      set: (key: string, value?: string) => new Promise<void>((resolve, reject) =>
        storage.setItem(key, value || '', err => err ? reject(err) : resolve())),
      get: (key: string) => new Promise<string | undefined>((resolve, reject) =>
        storage.getItem(key, (err, val) => err ? reject(err) : resolve(val === null ? undefined : val))),
      setObject: (key: string, value: object) => new Promise<void>((resolve, reject) =>
        storage.setItem(key, JSON.stringify(value), err => err ? reject(err) : resolve())),
      getObject: (key: string) => new Promise<object | undefined>((resolve, reject) =>
        storage.getItem(key, (err, data) => err ? reject(err) : resolve(data && JSON.parse(data)))),
      getAllKeys: () => new Promise<string[]>((resolve, reject) =>
        storage.getAllKeys((err, keys) => err ? reject(err) : resolve(keys || []))),
      remove: (key: string) => new Promise<void>((resolve, reject) =>
        storage.removeItem(key, err => err ? reject(err) : resolve()))
    };
  }
  init = async (region: string): Promise<boolean> => {
    if (this._initialized) return true;
    this._util = new CognitoUtil(region, this._mode, this._storage);
    this._reg = new UserRegistration(this._mode, this._util);
    this._profile = new UserProfile(this._util, this._storage);
    this._login = new UserLogin(region, this._profile, this._util, this._storage);
    await this._util.sync();
    this._initialized = true;
    return this._initialized;
  }
  authToken = async (tagAsExpired: boolean = false): Promise<string> => {
    const token = await this._storage.get('userTokens.idToken');
    return token === undefined ? 'Guest' : `Bearer ${tagAsExpired ? 'EXP ' : ''}${token}`;
  }
  userGroups = async (): Promise<string[]> => {
    if (!this._util) throw new Error('not initialized');
    return await this._util.getUserGroup();
  }
  signedIn = (): boolean => { throw new Error('signed in not implemented'); }
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
    const sub = await this.sub();
    const ret = await this._login.signOut();
    await this._storage.clear();
    //console.log(`SIGN-OUT ${sub.ok}`);
    //Sentry.setUserContext({ id: sub.ok });
  }
  currentUserState = async (): Promise<UserState> => {
    if (!this._util) throw new Error('not initialized');
    return await this._util.getUserState();
  }
  currentUser = async (): Promise<User> => {
    if (!this._util) throw new Error('not initialized');
    return await this._util.getCurrentUser();
  }
  userName = async (): Promise<string | undefined> => {
    if (!this._util) throw new Error('not initialized');
    return await this._util.getUserName();
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
  changePassword = async (previousPassword: string, proposedPassword: string): Promise<void> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.changePassword(previousPassword, proposedPassword);
  }
  completeNewPasswordChallenge = async (newPassword: string, locale: string): Promise<any> => {
    if (!this._login) throw new Error('not initialized');
    return await this._login.completeNewPasswordChallenge(newPassword, locale);
  }
  userAttributes = async (): Promise<Obj<string>> => {
    if (!this._profile) throw new Error('not initialized');
    return await this._profile.getUserAttributes();
  }
  setUserAttributes = async (attributes: Obj<string>): Promise<string> => {
    if (!this._profile) throw new Error('not initialized');
    return await this._profile.setUserAttributes(attributes);
  }
  attributeVerificationCode = async (attribute: string): Promise<object> => {
    if (!this._profile) throw new Error('not initialized');
    return await this._profile.getAttributeVerificationCode(attribute);
  }
  verifyAttribute = async (attribute: string, verificationCode: string): Promise<string> => {
    if (!this._profile) throw new Error('not initialized');
    return await this._profile.verifyAttribute(attribute, verificationCode);
  }
  refreshCredentials = async (): Promise<void> => {
    if (!this._util || !this._profile || !this._login) throw new Error('not initialized');
    const util = this._util;
    const profile = this._profile;
    const login = this._login;
    // Workaround: aws cognito user device tracking is not used by this application. however react-native-aws-cognito-js@0.0.4
    // does not expose an easy way to disable this. by surveying its sourcecode, clearing out the LastAuthUser value from
    // the storage used by the cognito-user object will fix.
    const user = await this._util.getCurrentUser();
    const clientId = this._mode == UserPoolMode.Web ? outputs.WebUserPoolClientId : outputs.MobileUserPoolClientId;
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
            await this._storage.set('userTokens.awsAccessKeyId', Client.accessKeyId());
            await this._storage.set('userTokens.awsSecretAccessKey', Client.secretAccessKey());
            await this._storage.set('userTokens.awsSessionToken', Client.sessionToken());
            resolve();
          } catch (err) {
            reject(err);
          }
        });
      });
    });
  }
  credentials = async (): Promise<AccountCredentials | undefined> => {
    if (!this._login) throw new Error('not initialized');
    this._login.getAwsCredentials();
    const accessKeyId = await this._login.getAwsAccessKey();
    const secretAccessKey = await this._login.getAwsSecretAccessKey();
    const sessionToken = await this._login.getAwsSessionToken();
    if (accessKeyId === undefined || secretAccessKey === undefined || sessionToken === undefined)
      return undefined;
    return { accessKeyId, secretAccessKey, sessionToken };
  }
  sub = async (): Promise<string | undefined> => {
    if (!this._profile) throw new Error('not initialized');
    const attrs = await this._profile.getUserAttributes();
    return attrs.sub;
  }
}
