import { createModule, useGlobals } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';

export const useCognito = (props: {
  Domain: string,
  InvitationEmailSubject: string,
  VerificationEmailSubject: string,
  FromEmail: string,
}) => createModule(async () => {
  const { stage } = useGlobals();

  const ret = await useCloudFormation({
    id: 'cf-cognito',
    cfyamlpath: `${__dirname}/cf.yaml`,
    inputs: {
      ...props,
      Stage: stage
    },
    defaultOutputs: {
      CognitoIdentityPoolId: '',
      UserPoolId: '',
      WebUserPoolClientId: '',
      MobileUserPoolClientId: ''
    }
  });

  return ret;
});