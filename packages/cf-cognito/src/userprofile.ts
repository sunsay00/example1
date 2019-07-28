import Client, { CognitoUserAttribute } from './client';
import CognitoUtil from './cognitoutil';
import { LocalStorage, Obj } from './types';

export default class UserProfile {
  private _util: CognitoUtil;
  private _localStorage: LocalStorage;

  constructor(util: CognitoUtil, localStorage: LocalStorage) {
    this._localStorage = localStorage;
    this._util = util;
  }

  setUserAttributes = async (attributes: Obj<string>): Promise<string> => {
    return new Promise<string>(async (resolve, reject) => {
      const attributeList = Object.entries(attributes)
        .map(([key, value]) => {
          if (value != undefined) {
            return Client.createCognitoUserAttribute(key, value);
          }
        });
      const cognitoUser = await this._util.getCognitoUser();
      if (cognitoUser === undefined) return reject('user currently not logged in');
      cognitoUser.getSession((err: Error, session: any) => {
        if (err) { return reject(err); }
        cognitoUser.updateAttributes(attributeList, (err: Error, result: string): void => {
          if (err) { return reject(err); }
          resolve(result);
        });
      });
    });
  }
  getAttributeVerificationCode = async (attribute: string): Promise<object> => {
    return new Promise<object>(async (resolve, reject) => {
      const cognitoUser = await this._util.getCognitoUser();
      if (cognitoUser === undefined) return reject('user currently not logged in');
      cognitoUser.getSession((err: Error, session: any) => {
        if (err) { reject(err); return; }
        cognitoUser.getAttributeVerificationCode('email', {
          onFailure: err => reject(err),
          inputVerificationCode: (data: object) => { resolve(data); },
        });
      });
    });
  }
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
      if (cret != undefined) {
        resolve(cret as Obj<string>);
      } else {
        const cognitoUser = await this._util.getCognitoUser();
        if (cognitoUser === undefined) return reject('user currently not logged in');
        cognitoUser.getSession((err: Error, session: any) => {
          if (err) { return reject(err); }
          cognitoUser.getUserAttributes(async (err: Error, result: CognitoUserAttribute[]) => {
            if (err) { return reject(err); }
            const userAttributes = result.reduce((ret, p) => ({ ...ret, [p.getName()]: p.getValue() }), {});
            console.log('Cognito User Pools User Attributes:', userAttributes);
            // Write user profile attributes to local storage
            await this._localStorage.setObject('userProfile', userAttributes);
            resolve(userAttributes);
          });
        });
      }
    });
  }
}