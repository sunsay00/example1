import { IUserContext, INotificationManager, IDBClient, ICacheClient } from './types';
import { MappedServices } from './back/api/src/api/mapper';
import { Resolver } from './tools/resolver';
export * from './types';
export * from './back/api/src/types/serviceinterfaces';
export declare const createCachedResolver: <C extends IUserContext>(stage: string, notifications: INotificationManager<C>, cache: ICacheClient, db: IDBClient, services: MappedServices<C>) => Resolver<C>;
export declare const createResolver: <C extends IUserContext>(stage: string, notifications: INotificationManager<C>, db: IDBClient, services: MappedServices<C>) => Resolver<C>;
//# sourceMappingURL=index.d.ts.map