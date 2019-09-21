import CognitoUtil from './cognitoutil';
import { CognitoClient, LocalStorage, Obj, CognitoUserAttribute } from './types';

export default class UserProfile {
  private _client: CognitoClient;
  private _util: CognitoUtil;
  private _localStorage: LocalStorage;

  constructor(client: CognitoClient, util: CognitoUtil, localStorage: LocalStorage) {
    this._client = client;
    this._localStorage = localStorage;
    this._util = util;
  }

  setUserAttributes = async (attributes: Obj<string>): Promise<string> => {
    return new Promise<string>(async (resolve, reject) => {
      const attributeList = Object.entries(attributes)
        .map(([key, value]) => value && this._client.createCognitoUserAttribute(key, value) || undefined)
        .filter(x => x != undefined);
      const cognitoUser = await this._util.getCognitoUser();
      if (cognitoUser === undefined) return reject('user currently not logged in');
      cognitoUser.getSession((err: Error, session: any) => {
        if (err) { return reject(err); }
        cognitoUser.updateAttributes(attributeList, (err?: Error, result?: string): void => {
          if (err) { return reject(err); }
          resolve(result);
        });
      });
    });
  }
  getAttributeVerificationCode = async (attribute: string) => new Promise<string>(async (resolve, reject) => {
    const cognitoUser = await this._util.getCognitoUser();
    if (cognitoUser === undefined) return reject('user currently not logged in');
    cognitoUser.getSession((err: Error, session: any) => {
      if (err) { reject(err); return; }
      cognitoUser.getAttributeVerificationCode('email', {
        onSuccess: () => resolve(),
        onFailure: err => reject(err),
        inputVerificationCode: (data: string) => resolve(data),
      });
    });
  });
  verifyAttribute = async (attribute: string, verificationCode: string): Promise<string> => {
    return new Promise<string>(async (resolve, reject) => {
      const cognitoUser = await this._util.getCognitoUser();
      if (cognitoUser === undefined) return reject('user currently not logged in');
      cognitoUser.getSession(function (err: Error, session: any) {
        if (err) { reject(err); return; }
        cognitoUser.verifyAttribute(attribute, verificationCode, {
          onFailure: err => reject(err),
          onSuccess: data => resolve(data),
        });
      });
    });
  }
  getUserAttributes = async (): Promise<Obj<string>> => {
    return new Promise<Obj<string>>(async (resolve, reject) => {
      const cret = await this._localStorage.getObject('userProfile');
      if (cret) {
        resolve(cret as Obj<string>);
      } else {
        const cognitoUser = await this._util.getCognitoUser();
        if (cognitoUser === undefined) return reject('user currently not logged in');
        cognitoUser.getSession((err: Error, session: any) => {
          if (err) { return reject(err); }
          cognitoUser.getUserAttributes(async (err?: Error, result?: CognitoUserAttribute[]) => {
            if (err || !result) { return reject(err); }
            const userAttributes = result.reduce((ret, p) => ({ ...ret, [p.getName()]: p.getValue() }), {});
            //console.log('Cognito User Pools User Attributes:', userAttributes);
            // Write user profile attributes to local storage
            await this._localStorage.setObject('userProfile', userAttributes);
            resolve(userAttributes);
          });
        });
      }
    });
  }
}