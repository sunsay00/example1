import CacheClient from '@inf/cf-redis';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { IUserContext, INotificationManager } from './types';
import { MappedServices } from './back/api/src/api/mapper';
import { Resolver } from './tools/resolver';
export * from './types';
export * from './back/api/src/types/serviceinterfaces';
export declare const createCachedResolver: <C extends IUserContext>(stage: string, notifications: INotificationManager<C>, cache: CacheClient, db: RDSDBClient, services: MappedServices<C>) => Resolver<C>;
export declare const createResolver: <C extends IUserContext>(stage: string, notifications: INotificationManager<C>, db: RDSDBClient, services: MappedServices<C>) => Resolver<C>;
//# sourceMappingURL=index.d.ts.map