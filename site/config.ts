import { createModule, useShell, useGlobals } from '@inf/hookops';

export const useSite = () => createModule('site', async () => {
  const { stage } = useGlobals();

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

