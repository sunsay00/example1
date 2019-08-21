import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { main } from './legacy/main';

const params = verifyVars({
  stage: process.env.STAGE,
  nodeEnv: process.env.NODE_ENV,
  corsAllowOrigin: '*',
});

export const handler = apiWrapper(params)(main);