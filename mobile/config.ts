import { createModule, useGlobals, useShell } from '@infng/hookops';
import { useFrontendGen, GenProps } from '@infng/cf-gen/config';
import { useVarsWriter, useGitIgnore } from '@infng/hooks';

export const useMobile = (inputs: {
  graphqlEndpoint: string
  region: string,
  cognito: {
    identityPoolId: string,
    userPoolId: string,
    clientId: string
  },
  genProps: GenProps
}) => createModule(async () => {
  const { currentModuleDir } = useGlobals();

  await useFrontendGen({
    genProps: inputs.genProps
  });

  useVarsWriter('ts', currentModuleDir, {
    GRAPHQL_ENDPOINT: inputs.graphqlEndpoint,
    AWS_REGION: inputs.region,
    IDENTITY_POOL_ID: inputs.cognito.identityPoolId,
    USER_POOL_ID: inputs.cognito.userPoolId,
    CLIENT_ID: inputs.cognito.clientId
  });

  return {};
});

