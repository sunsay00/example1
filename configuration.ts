import { createConfiguration } from '@infng/hookops';
import { vars } from '@infng/hookops/vars';

import { useSite } from './site/config';
import { useMobile } from './mobile/config';

const ROOT_ENV = './.env';

export default createConfiguration({
  stage: vars.STAGE,
  configure: async () => {

    console.log('configuration called');

    await useSite({
      graphqlEndpoint: 'localhost/graphql',
      region: vars.AWS_REGION,
      cognito: {
        identityPoolId: 'n/a',
        userPoolId: 'n/a',
        clientId: 'n/a'
      },
    });

    await useMobile({
      graphqlEndpoint: 'localhost/graphql',
      region: vars.AWS_REGION,
      cognito: {
        identityPoolId: 'n/a',
        userPoolId: 'n/a',
        clientId: 'n/a'
      },
    });
  }
});
