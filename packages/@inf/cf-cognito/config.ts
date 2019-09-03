import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  Domain: string,
  InvitationEmailSubject: string,
  VerificationEmailSubject: string,
  FromEmail: string,
}) => createConfigRecord(async ({ stage }) => ({
  type: 'cloudformation',
  rootDir: __dirname,
  cfpath: './cf.yaml',
  inputs: {
    ...inputs,
    Stage: stage
  },
  outputs: {
    CognitoIdentityPoolId: '',
    UserPoolId: '',
    WebUserPoolClientId: '',
    MobileUserPoolClientId: ''
  }
}));
