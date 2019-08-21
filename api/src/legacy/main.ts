//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { Variables, createResolver } from '@inf/cf-gen';
import NotificationManager from './tools/notificationmanager';

import { UserContext } from './types';
import UsersService from './services/users';
import SellersService from './services/sellers';
import SystemService from './services/system';
import DeviceListsService from './services/devicelists';

const config = {
  AWS_RDS_DB_ENDPOINT: '',
  REDIS_URL: '',
  PLATFORM_APPLICATION_ARN: '',
  STAGE: 'dev',
  LOCALE: 'en',
  REGION: 'us-east-1',
};

const cache = undefined;//new CacheClient(config.REDIS_URL);
const notifications = new NotificationManager(config.STAGE, config.REGION, config.PLATFORM_APPLICATION_ARN, cache);
const db = new RDSDBClient(config.AWS_RDS_DB_ENDPOINT);

const resolver = createResolver(config.STAGE, notifications, db, {
  users: store => new UsersService(notifications, store, config.LOCALE, config.REGION),
  systems: store => new SystemService(notifications, store),
  deviceLists: store => new DeviceListsService(notifications, store),
  sellers: store => new SellersService(notifications, store),
});

export const main = async (headers: { [_: string]: string }, query: string, variables: Variables, $ctx: UserContext) => {
  console.log('Main Main');
  return resolver.resolve(headers, query, variables, $ctx);
};