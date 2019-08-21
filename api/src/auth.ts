import { authWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { vars } from '@inf/cf-cognito/src/vars';

const params = verifyVars({
  awsRegion: process.env.AWS_REGION,
  nodeEnv: process.env.NODE_ENV,
  userPoolId: vars.UserPoolId,
});

export const handler = authWrapper(params);