import Client from './client';
import { LocalStorage, UserState, UserPoolMode, UserPool, User } from './types';

export default class CognitoUtil {
  //private _prevUserState?: UserState;
  private _region: string;
  private _mode: UserPoolMode;
  private _localStorageService: LocalStorage;

  constructor(region: string, mode: UserPoolMode, localStorageService: LocalStorage) {
    this._region = region;
    this._mode = mode;
    this._localStorageService = localStorageService;
  }

  getCognitoIdentityId = (): string => {
    return Client.cognitoIdentityId();
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
    Client.setCognitoIdentityPoolDetails(this._region);
    return Client.createCognitoUserPool(this._mode);
  }

  sync = async (): Promise<boolean> => {
    return new Promise<boolean>(async (resolve, reject) => {
      const username = await this._localStorageService.get('userName');
      if (username === undefined) { return resolve(false); }
      Client.createCognitoUser(this._region, this._mode, username).storage.sync(async (err: any, result: any) => {
        if (err) { return reject(err); }
        resolve(true);
      });
    });
  }

  getCognitoUser = async (): Promise<User | undefined> => {
    const username = await this._localStorageService.get('userName');
    if (username === undefined) return undefined;
    return Client.createCognitoUser(this._region, this._mode, username);
  }

  getCurrentUser = () => {
    return Client.currentUser(this._region, this._mode);
  }
}
