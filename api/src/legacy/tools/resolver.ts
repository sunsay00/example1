import { createResolver } from '@inf/cf-gen';
import NotificationManager from './notificationmanager';
import UsersService from '../services/users';
import SellersService from '../services/sellers';
import SystemService from '../services/system';
import DeviceListsService from '../services/devicelists';

import { ICacheClient, IDBClient } from '@inf/cf-gen';

export const createDefaultResolver = (params: {
  stage: string,
  region: string,
  locale: string,
  platformApplicationArn: string,
  db: IDBClient,
  cache?: ICacheClient,
  onInit?: () => Promise<void>,
}) => {
  const notifications = new NotificationManager(
    params.stage,
    params.region,
    params.platformApplicationArn,
    params.cache);
  return createResolver({
    stage: params.stage,
    notifications,
    db: params.db,
    onInit: params.onInit,
    services: {
      users: store => new UsersService(notifications, store, params.locale, params.region),
      systems: store => new SystemService(notifications, store),
      deviceLists: store => new DeviceListsService(notifications, store),
      sellers: store => new SellersService(notifications, store),
    },
  });
};
