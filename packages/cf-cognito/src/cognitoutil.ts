import { CognitoClient, LocalStorage, UserState, UserPoolMode, UserPool, CognitoUser } from './types';

export default class CognitoUtil {
  private _client: CognitoClient;
  private _region: string;
  private _mode: UserPoolMode;
  private _localStorageService: LocalStorage;

  constructor(client: CognitoClient, region: string, mode: UserPoolMode, localStorageService: LocalStorage) {
    this._client = client;
    this._region = region;
    this._mode = mode;
    this._localStorageService = localStorageService;
  }

  getCognitoIdentityId = (): string => {
    return this._client.cognitoIdentityId();
  }

  getUserName = async (): Promise<string | undefined> => {
    // Retrieve username from local storage. Return null if it does not exist
    return this._localStorageService.get('userName');
  }

  setUsername = async (username: string): Promise<void> => {
    return this._localStorageService.set('userName', username);
  }

  getUserId = async (): Promise<string | undefined> => {
    // Retrieve user ID from local storage. Return null if it does not exist
    return this._localStorageService.get('userId');
  }

  getUserProfile = async (): Promise<object> => {
    // Retrieve user profile attributes from local storage
    const ret = await this._localStorageService.getObject('userProfile');
    return ret == undefined ? {} : ret;
  }

  getUserGroup = async (): Promise<string[]> => {
    // Retrieve the user group from the local storage
    const groups = await this._localStorageService.get("userGroup");
    if (groups == undefined) return [];
    return groups.split(',');
  }

  getUserState = async (): Promise<UserState> => {
    // Retrieve user state from local storage. Return null if it does not exist
    const state = await this._localStorageService.get('userState');
    switch (state) {
      case 'SignedOut': return state;
      case 'SignedIn': return state;
      case 'PendingConfirmation': return state;
      case 'InvalidCredentials': return state;
      default: return 'SignedOut';
    }
  };

  setUserState = async (userState: UserState): Promise<void> => {
    return this._localStorageService.set('userState', userState);
  }

  getUserPool = (): UserPool => {
    this._client.setCognitoIdentityPoolDetails(this._region);
    return this._client.createCognitoUserPool(this._mode);
  }

  sync = async (): Promise<boolean> => {
    return new Promise<boolean>(async (resolve, reject) => {
      const username = await this._localStorageService.get('userName');
      if (username === undefined) { return resolve(false); }
      const user = this._client.createCognitoUser(this._region, this._mode, username);
      if ((user as any).storage) {
        (user as any).storage.sync(async (err: any, result: any) => {
          if (err) { return reject(err); }
          resolve(true);
        });
      } else {
        console.warn('cognito user stoage is undefined');
      }
    });
  }

  getCognitoUser = async (): Promise<CognitoUser | undefined> => {
    const username = await this._localStorageService.get('userName');
    if (username === undefined) return undefined;
    return this._client.createCognitoUser(this._region, this._mode, username);
  }

  getCurrentUser = () => {
    return this._client.currentUser(this._region, this._mode);
  }
}
