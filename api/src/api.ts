import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from './legacy/tools/resolver';

//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';

const config = verifyVars({
  stage: process.env.STAGE,
  nodeEnv: process.env.NODE_ENV,
  corsAllowOrigin: '*',
  region: 'us-east-1',
  locale: 'en',
  platformApplicationArn: '',
  rdsDbEndpoint: '',
});

const resolver = createDefaultResolver({
  stage: config.stage,
  region: config.region,
  locale: config.locale,
  platformApplicationArn: config.platformApplicationArn,
  db: new RDSDBClient(config.rdsDbEndpoint),
  cache: undefined, // new CacheClient(config.redisUrl),
});

export const handler = apiWrapper(config)(resolver.resolve);