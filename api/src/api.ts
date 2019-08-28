import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from './legacy/tools/resolver';
//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { vars } from '@inf/cf-gen/vars';

const config = verifyVars({
  stage: process.env.STAGE,
  corsAllowOrigin: '*',
  region: process.env.AWS_REGION,
  locale: 'en',
  platformApplicationArn: 'NYI',
});

const resolver = createDefaultResolver({
  stage: config.stage,
  region: config.region,
  locale: config.locale,
  platformApplicationArn: config.platformApplicationArn,
  db: new RDSDBClient(vars.DB_URL),
  cache: undefined, // new CacheClient(config.redisUrl),
});

export const handler = apiWrapper(config)(resolver.resolve);