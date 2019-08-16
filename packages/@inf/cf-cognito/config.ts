import { ConfigRecord } from '@inf/configure';

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
  type: 'cloudformation' as const,
  name: 'cf-cognito',
  key: 'COG',
  inputs,
  outputs: ['IdentityPoolId', 'UserPoolId', 'WebUserPoolClientId', 'MobileUserPoolClientId'] as (keyof Outputs)[]
});
