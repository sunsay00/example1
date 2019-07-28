import { useState, useEffect } from 'react';
import { config as AWSConfig, CognitoIdentityCredentials } from 'aws-sdk';
//import { AuthenticationDetails, CognitoUserSession, CognitoUserPool, CognitoUser } from 'amazon-cognito-identity-js';
//import awsexports from '../aws-exports';

const cognitoauth = (region: string, email: string, username: string, password: string) => {
  return new Promise<string>((resolve, reject) => {
    resolve('');
    /*
    try {
      AWSConfig.region = region,
        AWSConfig.credentials = new CognitoIdentityCredentials({
          IdentityPoolId: awsexports.CognitoIdentityPoolId,
        });
      const userPool = new CognitoUserPool({
        UserPoolId: awsexports.UserPoolId,
        ClientId: awsexports.UserPoolWebClientId
      });
      const cognitoUser = new CognitoUser({
        Username: username,
        Pool: userPool,
      });
      let forceChangePassword = false; // addresses FORCE_CHANGE_PASSWORD flow for a newly created user in the aws console
      const authenticationDetails = new AuthenticationDetails({
        Username: username,
        Password: password,
      });

      //console.log('***username:', username, 'password:', password);
      cognitoUser.authenticateUser(authenticationDetails, {
        newPasswordRequired: function (userAttributes: any, requiredAttributes: any) {
          const newPassword = password.split('').reverse().join('');
          userAttributes.email = email;
          userAttributes.locale = 'en';
          userAttributes.preferred_username = username,
            delete userAttributes.email_verified; // the api doesn't accept this field back
          forceChangePassword = true;
          //console.log('***new password required', password, '->', newPassword);
          cognitoUser.completeNewPasswordChallenge(newPassword, userAttributes, this);
        },
        customChallenge: (challengeParameters: any) => {
          //console.log('***customchallenge');
          reject(new Error('custom challenge received'));
        },
        mfaRequired: (challengeName: any, challengeParameter: any) => {
          //console.log('***mfarequired');
          reject(new Error('mfa required'));
        },
        onSuccess: (session: CognitoUserSession) => {
          if (forceChangePassword) {
            const newPassword = password.split('').reverse().join('');
            //console.log('***success force change password', newPassword);
            cognitoUser.changePassword(newPassword, password, err => {
              if (err) { reject(new Error('failed to revert password')); return; }
              //console.log('***success password changed');
              resolve(session.getIdToken().getJwtToken());
            });
          } else {
            //console.log('***success');
            resolve(session.getIdToken().getJwtToken());
          }
        },
        onFailure: (err: any) => {
          //console.log('***error');
          reject(err);
        },
      });
    } catch (err) {
      reject(err);
    }
    */
  });
};

export const useAuthToken = (params: { region: string, email: string, username: string, password: string }): [string | undefined, () => Promise<string>] => {
  const [authToken, setAuthToken] = useState<string | undefined>(undefined);

  useEffect(() => {
    cognitoauth(params.region, params.email, params.username, params.password)
      .then(token => setAuthToken(token))
      .catch(console.error)
  }, []);

  if (!authToken)
    return [undefined, () => Promise.reject(new Error('token not initialized'))];
  else {
    return [
      authToken,
      async () => {
        const token = await cognitoauth(params.region, params.email, params.username, params.password)
        setAuthToken(token);
        return token;
      }
    ];
  }
}