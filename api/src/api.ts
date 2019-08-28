import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { vars } from '@inf/cf-gen/vars';
import { createDefaultResolver } from '../src/legacy/tools/resolver';

const config = verifyVars({
  stage: process.env.STAGE,
  corsAllowOrigin: '*',
  region: process.env.AWS_REGION,
  locale: 'en',
  platformApplicationArn: 'NYI',
});

const cache = undefined; // new CacheClient(config.redisUrl);

const db = new RDSDBClient(vars.DB_URL);

const resolver = createDefaultResolver({
  stage: config.stage,
  region: config.region,
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache,
  onPreResolve: async () => {
    await db.init();
  }
});

export const handler = apiWrapper(config)(resolver.resolve);