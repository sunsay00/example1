import { apiWrapper } from '@inf/core';
import { verifyVars } from '@inf/common';
//import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { vars } from '@inf/cf-gen/vars';
import { createResolver } from '@inf/cf-gen';
import NotificationManager from './legacy/tools/notificationmanager';
import UsersService from './legacy/services/users';
import SellersService from './legacy/services/sellers';
import SystemService from './legacy/services/system';
import DeviceListsService from './legacy/services/devicelists';

const config = verifyVars({
  stage: process.env.STAGE,
  corsAllowOrigin: '*',
  region: process.env.AWS_REGION,
  locale: 'en',
  platformApplicationArn: 'NYI',
});

const cache = undefined; // new CacheClient(config.redisUrl);

const db = new RDSDBClient(vars.DB_URL);

const notifications = new NotificationManager({
  stage: config.stage,
  region: config.region,
  platformApplicationArn: 'NYI',
  cache
});

const resolver = createResolver({
  stage: config.stage,
  notifications,
  db,
  onInit: async () => {
    await db.init();
  },
  services: {
    users: store => new UsersService(notifications, store, config.locale, config.region),
    systems: store => new SystemService(notifications, store),
    deviceLists: store => new DeviceListsService(notifications, store),
    sellers: store => new SellersService(notifications, store),
  },
});


export const handler = apiWrapper(config)(resolver.resolve);