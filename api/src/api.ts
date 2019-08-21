import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';

const params = verifyVars({
  stage: process.env.STAGE,
  nodeEnv: process.env.NODE_ENV,
  corsAllowOrigin: '*',
});

export const handler = apiWrapper(params, async (headers: { [_: string]: string }, query: string, variables: string, user: {}) => {
  console.log('Main Main');
  return {
    errors: [] as Error[],
    data: { no: 'wae' }
  }
});