import RDSStore from './back/api/src/api/stores/rdsstore';
import CacheStore from './back/api/src/api/stores/cachestore';
import { IUserContext, INotificationManager, IDBClient, ICacheClient } from './types';
import { createServiceMapper, MappedServices } from './back/api/src/api/mapper';
import { Resolver } from './tools/resolver';
import * as fix from './back/api/src/api/__tests__/fixtures';

export * from './types';
export * from './back/api/src/types/serviceinterfaces';
export const fixtures = fix;

export const createCachedResolver = <C extends IUserContext>(
  stage: string,
  notifications: INotificationManager<C>,
  cache: ICacheClient,
  db: IDBClient,
  services: MappedServices<C>,
  onInit?: () => Promise<void>,
) => {
  return new Resolver(stage, createServiceMapper(notifications, new CacheStore(new RDSStore(stage, db), cache), services), onInit);
};

export const createResolver = <C extends IUserContext>(params: {
  stage: string,
  notifications: INotificationManager<C>,
  db: IDBClient,
  services: MappedServices<C>,
  onInit?: () => Promise<void>,
}) => {
  return new Resolver(params.stage, createServiceMapper(params.notifications, new RDSStore(params.stage, params.db), params.services), params.onInit);
}