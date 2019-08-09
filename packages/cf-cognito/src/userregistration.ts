import CognitoUtil from './cognitoutil';
import { CognitoUserAttribute, UserPoolMode, CognitoClient } from './types';
import { vars } from './vars';

export default class UserRegistration {
  private _client: CognitoClient;
  private _mode: UserPoolMode;
  private _util: CognitoUtil;

  constructor(client: CognitoClient, mode: UserPoolMode, util: CognitoUtil) {
    this._client = client;
    this._mode = mode;
    this._util = util;
  }

  signUp = async (username: string, email: string, password: string, locale: string, role: string): Promise<string> => {
    return new Promise<string>((resolve, reject) => {
      const attributeList: CognitoUserAttribute[] = [];

      const attributePreferredUsername = this._client.createCognitoUserAttribute('preferred_username', username);
      attributeList.push(attributePreferredUsername);

      const attributeEmail = this._client.createCognitoUserAttribute('email', email);
      attributeList.push(attributeEmail);

      const attributeLocale = this._client.createCognitoUserAttribute('locale', locale);
      attributeList.push(attributeLocale);

      if (role != undefined) {
        const attributeRole = this._client.createCognitoUserAttribute('custom:role', role);
        attributeList.push(attributeRole);
      }

      this._util.getUserPool().signUp(username, password, attributeList, [], async (err, result) => {
        if (err || !result) { return reject(err); }

        console.log(`Username is ${result.user.getUsername()}`);
        console.log('Sign-up successful!');

        // Update user state to 'pendingConfirmation'
        await this._util.setUsername(username);
        await this._util.setUserState('PendingConfirmation');

        // Sign-up successful. Callback without error.
        resolve(result.user.getUsername());
      });
    });
  }

  confirmSignUp = async (confirmationCode: string): Promise<boolean> => {
    const cognitoUser = await this._util.getCognitoUser();
    return new Promise<boolean>((resolve, reject) => {
      if (cognitoUser === undefined) return reject('failed to get user');
      cognitoUser.confirmRegistration(confirmationCode, true, async (err: Error, data: string) => {
        if (err) { return reject(err); }
        await this._util.setUserState('SignedOut');
        resolve(data == 'SUCCESS');
      });
    });
  }

  resendConfirmationCode = async (): Promise<void> => {
    const Username = await this._util.getUserName();
    if (Username === undefined) throw new Error('failed to resend confirmation code');
    const ClientId = this._mode == UserPoolMode.Web ? vars.WebUserPoolClientId : vars.MobileUserPoolClientId;
    const cognitoParams = { ClientId, Username };
    await this._client.createCognitoServiceProvider().resendConfirmationCode(cognitoParams).promise();
  }
}
