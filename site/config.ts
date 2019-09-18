import { createModule, useShell, useGlobals } from '@inf/hookops';
import { useFrontendGen, GenProps } from '@inf/cf-gen/config';
import { useVarsWriter } from '@inf/hooks';

export const useSite = (props: {
  graphqlEndpoint: string
  region: string,
  cognito: {
    identityPoolId: string,
    userPoolId: string,
    clientId: string
  },
  genProps: GenProps
}) => createModule(async () => {
  const { stage } = useGlobals();

  await useFrontendGen({
    genProps: props.genProps
  });

  useVarsWriter('ts', __dirname, {
    GRAPHQL_ENDPOINT: props.graphqlEndpoint,
    AWS_REGION: props.region,
    IDENTITY_POOL_ID: props.cognito.identityPoolId,
    USER_POOL_ID: props.cognito.userPoolId,
    CLIENT_ID: props.cognito.clientId
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

