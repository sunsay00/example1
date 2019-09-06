import { createConfig } from '@inf/vars/configure';

export const Config = (inputs: {
  Domain: string,
  InvitationEmailSubject: string,
  VerificationEmailSubject: string,
  FromEmail: string,
}) => createConfig(({ stage }) => ({
  clean: async () => ({
    CognitoIdentityPoolId: '',
    UserPoolId: '',
    WebUserPoolClientId: '',
    MobileUserPoolClientId: ''
  }),
  up: async () => ({
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
  })
}));
