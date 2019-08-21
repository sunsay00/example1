import RDSDBClient from '@inf/cf-serverless-postgres';
import { IStore, IUserContext, ICacheClient } from '../types';
export declare const runCacheStoreSmokeTests: <C extends IUserContext>(store: IStore<C>, client: ICacheClient) => Promise<string>;
export declare const runRDSDBStoreSmokeTests: (client: RDSDBClient) => Promise<string>;
//# sourceMappingURL=smoketester.d.ts.map