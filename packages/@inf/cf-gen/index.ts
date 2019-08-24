import RDSStore from './back/api/src/api/stores/rdsstore';
import CacheStore from './back/api/src/api/stores/cachestore';
import { IUserContext, INotificationManager, IDBClient, ICacheClient } from './types';
import { createServiceMapper, MappedServices } from './back/api/src/api/mapper';
import { Resolver } from './tools/resolver';

export * from './types';
export * from './back/api/src/types/serviceinterfaces';

export const createCachedResolver = <C extends IUserContext>(
  stage: string,
  notifications: INotificationManager<C>,
  cache: ICacheClient,
  db: IDBClient,
  services: MappedServices<C>,
) =>
  new Resolver(stage, createServiceMapper(notifications, new CacheStore(new RDSStore(stage, db), cache), services));

export const createResolver = <C extends IUserContext>(
  stage: string,
  notifications: INotificationManager<C>,
  db: IDBClient,
  services: MappedServices<C>,
) =>
  new Resolver(stage, createServiceMapper(notifications, new RDSStore(stage, db), services));