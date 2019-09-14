import { createModule, useShell, useGlobals } from '@inf/hookops';
import { useVarsWriter } from '@inf/hooks';

export const useSite = (inputs: {
  graphqlEndpoint: string
  region: string,
  cognito: {
    identityPoolId: string,
    userPoolId: string,
    clientId: string
  }
}) => createModule(async () => {
  const { stage } = useGlobals();

  useVarsWriter('ts', __dirname, {
    GRAPHQL_ENDPOINT: inputs.graphqlEndpoint,
    AWS_REGION: inputs.region,
    IDENTITY_POOL_ID: inputs.cognito.identityPoolId,
    USER_POOL_ID: inputs.cognito.userPoolId,
    CLIENT_ID: inputs.cognito.clientId
  });

  if (stage != 'local') {
    await useShell({
      command: './site/make',
      args: ['deploy'],
      dependsOn: [
        './site/src/**/*.(ts|tsx|css)',
        './site/serverless.yml',
        './site/*.js',
        './site/static/**/*'
      ],
      //outputMatchers: { SiteURL: /Serverless: Success! Your site should be available at (.*)/ }
    });
  }

  return {};
});

