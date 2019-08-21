import { authWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { vars } from '@inf/cf-cognito/src/vars';

export const handler = authWrapper(verifyVars({
  awsRegion: process.env.AWS_REGION,
  nodeEnv: process.env.NODE_ENV,
  userPoolId: vars.UserPoolId,
}));