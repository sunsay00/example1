import * as sjcl from 'sjcl';
import CognitoUtil from './cognitoutil';
import UserProfileService from './userprofile';
import { CognitoClient, Obj, LocalStorage, CognitoUser, CognitoUserSession } from './types';
import { outputs } from './_outputs';

export default class UserLogin {
  private _client: CognitoClient;
  private _region: string;
  private _profile: UserProfileService;
  private _util: CognitoUtil;
  private _accessToken?: string;
  private _idToken?: string;
  private _refreshToken?: string;
  private _localStorage: LocalStorage;
  private _currentSignInUser?: CognitoUser;

  constructor(client: CognitoClient, region: string, profile: UserProfileService, util: CognitoUtil, localStorage: LocalStorage) {
    this._client = client;
    this._region = region;
    this._profile = profile;
    this._util = util;
    this._localStorage = localStorage;
  }

  getAccessToken = async (): Promise<string> => {
    let accessToken = this._accessToken;
    if (accessToken === undefined) {
      // retrieve from Local Storage if it exists
      accessToken = await this._localStorage.get('userTokens.accessToken');
      if (accessToken === undefined) {
        throw new Error('failed to get access token');
      }
      this._accessToken = accessToken;
    }
    return accessToken;
  };

  getIdToken = async (): Promise<string> => {
    let idToken = this._idToken;
    if (idToken === undefined) {
      // retrieve from Local Storage if it exists
      idToken = await this._localStorage.get('userTokens.idToken');
      if (idToken === undefined) {
        throw new Error('failed to get id token');
      }
      this._idToken = idToken;
    }
    return idToken;
  };

  getRefreshToken = async (): Promise<string> => {
    let refreshToken = this._refreshToken;
    if (refreshToken === undefined) {
      // retrieve from Local Storage if it exists
      refreshToken = await this._localStorage.get('userTokens.refreshToken');
      if (refreshToken === undefined) {
        throw new Error('failed to get refresh token');
      }
      this._refreshToken = refreshToken;
    }
    return refreshToken;
  }

  getAwsAccessKey = async (): Promise<string | undefined> => {
    return this._client.accessKeyId() || await this._localStorage.get('userTokens.awsAccessKeyId');
  }

  getAwsSecretAccessKey = async (): Promise<string | undefined> => {
    return this._client.secretAccessKey() || await this._localStorage.get('userTokens.awsSecretAccessKey');
  }

  getAwsSessionToken = async (): Promise<string | undefined> => {
    return this._client.sessionToken() || await this._localStorage.get('userTokens.awsSessionToken');
  }

  private clearUserState = async (): Promise<void> => {
    // Clear user tokens
    this._accessToken = undefined;
    this._idToken = undefined;
    this._refreshToken = undefined;

    await this._localStorage.set('userTokens.accessToken', undefined);
    await this._localStorage.set('userTokens.idToken', undefined);
    await this._localStorage.set('userTokens.refreshToken', undefined);
    await this._localStorage.set('userTokens.awsAccessKeyId', undefined);
    await this._localStorage.set('userTokens.awsSecretAccessKey', undefined);
    await this._localStorage.set('userTokens.awsSessionToken', undefined);

    // Clear user state
    await this._util.setUserState('SignedOut');

    // Clear user profile attributes
    await this._localStorage.set('userProfile', undefined);

    // Clear username and user ID attributes
    await this._localStorage.set('userId', undefined);
    await this._localStorage.set('userName', undefined);
  };

  signIn = async (emailOrUsername: string, password: string): Promise<'success' | 'changepassword'> => {
    const authenticationDetails = this._client.createAuthenticationDetails(emailOrUsername, password);

    // set username (could also be email at this point)
    await this._util.setUsername(emailOrUsername);

    let user = this._currentSignInUser = await this._util.getCognitoUser();
    if (user === undefined) throw new Error('failed to get user');
    const cognitoUser = user;

    console.log('Authenticating user ' + emailOrUsername);

    return await new Promise<'success' | 'changepassword'>((resolve, reject) => {
      cognitoUser.authenticateUser(authenticationDetails, {
        newPasswordRequired: (userAttributes: any, requiredAttributes: any) => {
          resolve('changepassword');
        },
        customChallenge: (challengeParameters: any) => { reject('custom challenge'); },
        mfaRequired: (challengeName: any, challengeParameter: any) => { reject('mfa required'); },
        onSuccess: async (session: CognitoUserSession) => {
          try {
            // set username again (this time, the username is the username, recevied from the server)
            await this._util.setUsername(cognitoUser.getUsername());

            //console.log('SESSION:', JSON.stringify(session));
            // Save user tokens to local state
            const accessToken = await session.getAccessToken().getJwtToken();
            const idToken = await session.getIdToken().getJwtToken();
            const refreshToken = await session.getRefreshToken().getToken();
            this._accessToken = accessToken;
            this._idToken = idToken;
            this._refreshToken = refreshToken;
            //console.log(`IDTOKEN ${this._idToken} - ${idToken}`);
            await this._localStorage.set('userTokens.idToken', this._idToken);
            //console.log('COGNITO User Pools Identity Token: ', await this.getIdToken());
            await this._localStorage.set('userTokens.accessToken', this._accessToken);
            //console.log('COGNITO User Pools Access Token: ', await this.getAccessToken());
            await this._localStorage.set('userTokens.refreshToken', this._refreshToken);
            //console.log('COGNITO User Pools Refresh Token: ', await this.getRefreshToken());

            // Extract the user group from the identity token.
            // First, get the identity token payload and then perform a Base64 decoding
            // so you can later extract the user group.
            if (!this._idToken || this._idToken.indexOf('.') === -1) {
              throw new Error('invalid id token');
            }
            let idTokenPayload = this._idToken.split('.')[1];
            let payload = JSON.parse(sjcl.codec.utf8String.fromBits(sjcl.codec.base64url.toBits(idTokenPayload)));
            let userGroup = payload["cognito:groups"];
            if (userGroup && userGroup.length > 0) {
              await this._localStorage.set('userGroup', userGroup.join(','));
            } else {
              // The user group is set only for the pre-defined users. By default
              // we assign them to client group.
              userGroup = 'clientGroup';
              await this._localStorage.set('userGroup', userGroup);
            }
            //console.log(`Cognito User Pools User Groups: ${emailOrUsername} belongs to group ${userGroup}`);

            const userAttrs = await this._profile.getUserAttributes();
            const role = userAttrs['custom:role'];
            //if (role != undefined) {
            //Root.setRole(role as UserRole);
            //}

            // Set user state to authenticated
            this._util.setUserState('SignedIn');

            // Read user attributes and write to console
            await this.getAwsCredentials();
            const accessKeyId = this._client.accessKeyId();
            const secretAccessKey = this._client.secretAccessKey();
            const sessionToken = this._client.sessionToken();
            if (accessKeyId && secretAccessKey && sessionToken) {
              //console.log('AWS Access Key ID: ', accessKeyId);
              await this._localStorage.set('userTokens.awsSecretAccessKey', secretAccessKey);
              //console.log('AWS Secret Access Key: ', secretAccessKey);
              await this._localStorage.set('userTokens.awsSessionToken', sessionToken);
              //console.log('AWS Session Token: ', sessionToken);
              await this._localStorage.set('userTokens.awsAccessKeyId', accessKeyId);
            } else {
              console.warn('failed to get temp credentials');
            }
            /*
            await this._profile.getUserAttributes();
            await this._localStorageService.set('userId', this._util.getCognitoIdentityId());
            console.log('Cognito Identity ID: ', this._util.getCognitoIdentityId());
            */
            resolve('success');
          } catch (err) {
            reject(err);
          }
        },
        onFailure: async (err: any) => {
          // Check for user not confirmed exception
          if (err.code === 'UserNotConfirmedException') {
            // Set user state to pending confirmation
            await this._util.setUserState('PendingConfirmation');
          } else {
            await this._util.setUserState('InvalidCredentials');
          }
          reject(err);
        }
      });
    });
  }

  signOut = async (): Promise<void> => {
    // Logout from Cognito service
    const cognitoUser = await this._util.getCognitoUser();
    // Clear local user state
    await this.clearUserState();
    if (cognitoUser !== undefined) {
      cognitoUser.signOut();
    }
    this._client.clearCachedId();
  }

  globalSignOut = async (): Promise<void> => {
    // Clear local user state
    await this.clearUserState();
    // Logout from Cognito service
    const cognitoUser = await this._util.getCognitoUser();
    if (cognitoUser !== undefined) {
      cognitoUser.globalSignOut({
        onSuccess: (msg: string) => { },
        onFailure: (err: Error) => { },
      });
    }
    this._client.clearCachedId();
  }

  changePassword = async (previousPassword: string, proposedPassword: string) => new Promise<string | undefined>(async (resolve, reject) => {
    // first, load the valid tokens cached in the local store, if they are available
    // see: https://github.com/aws/amazon-cognito-identity-js/issues/71
    const cognitoUser = await this._util.getCognitoUser();
    if (cognitoUser === undefined) return reject('not currently logged in');
    cognitoUser.getSession((err: Error, session: any) => {
      if (err) return reject(err);
      cognitoUser.changePassword(previousPassword, proposedPassword, (err?: Error, result?: string) => {
        if (err) return reject(err);
        resolve(result);
      });
    });
  });

  completeNewPasswordChallenge = async (newPassword: string, locale: string) => {
    const cognitoUser = this._currentSignInUser || this._util.getCurrentUser();
    this._currentSignInUser = undefined;
    if (cognitoUser == undefined) throw Error('not currently logged in');

    const attributeList: Obj<string> = { locale };

    return await new Promise((resolve, reject) => {
      cognitoUser.completeNewPasswordChallenge(newPassword, attributeList, {
        onSuccess: resolve,
        onFailure: reject,
        mfaRequired: (codeDeliveryDetails, params) => {
          console.error(JSON.stringify(codeDeliveryDetails));
          reject('mfa not supported');
        },
        customChallenge: params => { }
      });
    });
  }

  forgotPassword = async (username: string): Promise<void> => {
    // Set target username
    await this._util.setUsername(username);

    // Get Cognito User with session
    const cognitoUser = await this._util.getCognitoUser();
    return new Promise<void>((resolve, reject) => {
      if (cognitoUser === undefined) return reject('failed to get user');
      cognitoUser.forgotPassword({
        onSuccess: (result: any) => {
          console.log('Initiated reset password for username ' + username);
          resolve(result);
        },
        onFailure: (err: Error) => {
          console.log('Failed to initiate reset password for username ' + username);
          reject(err);
        },
        inputVerificationCode: (result: any) => {
          resolve(result);
        },
      });
    });
  }

  confirmForgotPassword = async (username: string, verificationCode: string, password: string): Promise<void> => {
    // Set target username
    await this._util.setUsername(username);

    // Get Cognito User with session
    const cognitoUser = await this._util.getCognitoUser();

    return new Promise<void>((resolve, reject) => {
      if (cognitoUser === undefined) return reject('failed to get user');
      cognitoUser.confirmPassword(verificationCode, password, {
        onSuccess: () => {
          console.log('Password successfully reset for username ' + username);
          resolve();
        },
        onFailure: (err) => {
          console.log('Password was not reset for username ' + username);
          console.log(`Error: ${err.name}. ${err.message}`);
          reject(err);
          return;
        }
      });
    });
  }

  getAwsCredentials = async (): Promise<CognitoIdentityCredentials> => {
    // TODO: Integrate this method as needed into the overall module
    const logins: { [_: string]: string } = {};

    return new Promise<CognitoIdentityCredentials>(async (resolve, reject) => {
      // Check if user session exists
      const cognitoUser = await this._util.getCognitoUser();
      if (cognitoUser === undefined) return reject('failed to get user');
      cognitoUser.getSession((err: Error, result: any) => {
        if (err) return reject(err);

        logins[`cognito-idp.${this._region}.amazonaws.com/${outputs.UserPoolId}`] = result.getIdToken().getJwtToken();

        // Add the User's Id token to the Cognito credentials login map
        const credentials = this._client.setCognitoIdentityPoolDetails(logins);

        // Call refresh method to authenticate user and get new temp AWS credentials
        if (credentials.needsRefresh()) {
          credentials.clearCachedId();
          credentials.get((err) => {
            if (err) {
              console.error(err);
              return reject(err);
            }
            resolve(credentials);
          });
        } else {
          credentials.get((err: any) => {
            if (err) {
              console.error(err);
              return reject(err);
            }
            resolve(credentials);
          });
        }
      });
    });
  }
}
