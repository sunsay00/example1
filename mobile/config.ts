import { createModule, useGlobals, useShell } from '@inf/hookops';
import { useVarsWriter, useGitIgnore } from '@inf/hooks';

export const useMobile = (inputs: {
  graphqlEndpoint: string
  region: string,
  cognito: {
    identityPoolId: string,
    userPoolId: string,
    clientId: string
  }
}) => createModule(async () => {
  const { configurationDir, currentModuleDir } = useGlobals();

  await useShell({
    command: 'yarn',
    dependsOn: [`${configurationDir}/ledger.scm`],
    args: ['-s', 'x', 'gen', 'generateclient', '-o', `${currentModuleDir}/src/_gen`]
  });

  useGitIgnore(currentModuleDir, ['src/_gen']);

  useVarsWriter('ts', currentModuleDir, {
    GRAPHQL_ENDPOINT: inputs.graphqlEndpoint,
    AWS_REGION: inputs.region,
    IDENTITY_POOL_ID: inputs.cognito.identityPoolId,
    USER_POOL_ID: inputs.cognito.userPoolId,
    CLIENT_ID: inputs.cognito.clientId
  });

  return {};
});

