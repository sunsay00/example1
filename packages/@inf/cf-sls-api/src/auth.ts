import { authWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { vars } from './vars';

const params = verifyVars({
  awsRegion: process.env.AWS_REGION,
  userPoolId: vars.UserPoolId,
  stage: process.env.STAGE,
});

export const handler = authWrapper(params);