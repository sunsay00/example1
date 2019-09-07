import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import vars from './_vars';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import { APIGatewayProxyEvent, Context } from 'aws-lambda';

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

export const handler = (event: APIGatewayProxyEvent, context: Context, next: any) => {
  return apiWrapper(config)(resolver.resolve)(event, context, next);
}
