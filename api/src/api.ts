import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';

export const handler = apiWrapper(verifyVars({
  stage: process.env.STAGE,
  nodeEnv: process.env.NODE_ENV,
  corsAllowOrigin: '*',
}), async (headers: { [_: string]: string }, query: string, variables: string, user: {}) => {
  console.log('Main Main');
  return {
    errors: [] as Error[],
    data: { no: 'wae' }
  }
});