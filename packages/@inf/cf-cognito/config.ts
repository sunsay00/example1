import { createConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  Stage: string,
  Domain: string,
  InvitationEmailSubject: string,
  VerificationEmailSubject: string,
  FromEmail: string,
};

export type Outputs = {
  IdentityPoolId: string,
  UserPoolId: string,
  WebUserPoolClientId: string,
  MobileUserPoolClientId: string
};

export const Config = (inputs: Inputs) => createConfigRecord({
  type: 'cloudformation',
  name: 'cf-cognito',
  cfpath: './cf.yaml',
  inputs,
  outputs: ['CognitoIdentityPoolId', 'UserPoolId', 'WebUserPoolClientId', 'MobileUserPoolClientId'] as (keyof Outputs)[]
});
