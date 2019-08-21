import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import RDSStore from './back/api/src/api/stores/rdsstore';
import CacheStore from './back/api/src/api/stores/cachestore';
import { IUserContext, INotificationManager } from './types';
import { createServiceMapper, MappedServices } from './back/api/src/api/mapper';
import { Resolver } from './tools/resolver';

export * from './types';
export * from './back/api/src/types/serviceinterfaces';

export const createCachedResolver = <C extends IUserContext>(
  stage: string,
  notifications: INotificationManager<C>,
  cache: CacheClient,
  db: RDSDBClient,
  services: MappedServices<C>,
) =>
  new Resolver(stage, createServiceMapper(notifications, new CacheStore(new RDSStore(db), cache), services));

export const createResolver = <C extends IUserContext>(
  stage: string,
  notifications: INotificationManager<C>,
  db: RDSDBClient,
  services: MappedServices<C>,
) =>
  new Resolver(stage, createServiceMapper(notifications, new RDSStore(db), services));