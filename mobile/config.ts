import { createModule, useGlobals } from '@inf/hookops';
import { useVarsWriter } from '@inf/hooks';

export const useMobile = (inputs: {
  graphqlEndpoint: string
  region: string,
  cognito: {
    identityPoolId: string,
    userPoolId: string,
    clientId: string
  }
}) => createModule(async () => {
  const { currentModuleDir } = useGlobals();

  useVarsWriter('ts', currentModuleDir, {
    GRAPHQL_ENDPOINT: inputs.graphqlEndpoint,
    AWS_REGION: inputs.region,
    IDENTITY_POOL_ID: inputs.cognito.identityPoolId,
    USER_POOL_ID: inputs.cognito.userPoolId,
    CLIENT_ID: inputs.cognito.clientId
  });

  return {};
});

