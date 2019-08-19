import { ConfigRecord } from '@inf/vars/configure';

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

export const Config = (inputs: Inputs): ConfigRecord => ({
  type: 'cloudformation',
  name: 'cf-cognito',
  inputs,
  outputs: ['IdentityPoolId', 'UserPoolId', 'WebUserPoolClientId', 'MobileUserPoolClientId'] as (keyof Outputs)[]
});
