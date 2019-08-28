import * as M from './back/api/src/types/serviceinterfaces';
import { IStore } from './back/api/src/types/storeinterfaces';

export { IStore } from './back/api/src/types/storeinterfaces';
export { ITestingService } from './tools/testingservice';

export type JSONValue = string | number | boolean | JSONObject | JSONArray;
export type JSONObject = { [_: string]: JSONValue; }
export interface JSONArray extends Array<JSONValue> { }

export type Variables = { [_: string]: JSONValue | undefined; }

export type Dict<T> = {
  [_: string]: T | undefined;
  [_: number]: T | undefined;
}

export type Cursorize = {
  cursor?: string,
};

export type Cursored<T> = {
  cursor?: string,
  data: T,
}

export type Point = {
  lon: number,
  lat: number,
};

export type IUserContext = {
  sub: string,
  groups?: string[],
};

export type Paginated<T> = {
  cursor?: string,
  items: T[],
};

export type QueryResult = {
  command: string;
  rowCount: number;
  oid: number;
  rows: any[];
}

export type QueryParam = object | number | string | undefined;

export type IDBClient = {
  query(query: string, params: QueryParam[]): Promise<QueryResult>;
  beginTransaction(): Promise<QueryResult>,
  rollbackTransaction(): Promise<QueryResult>,
  commitTransaction(): Promise<QueryResult>,
  prepareString(value: unknown): string,
}

export type IMultiCacheClient = {
  set(key: string, value: string): void;
  get(key: string): void;
  del(keys: string[]): void;
  exists(key: string): void;
  hget(key: string, field: string): void;
  hmget(key: string, fields: string[]): void;
  hset(key: string, field: string, value: string): void;
  hmset(key: string, values: [string, string][]): void;
  expire(key: string, seconds: number): void;
  hdel(key: string, fields: string[]): void;
  zadd(key: string, values: [string, string][]): void;
  zremrangebyscore(key: string, minScore: string, inclusiveMaxScore: string): void;
  zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): void;
  zrevrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): void;
  zrem(key: string, members: string[]): void;
  geoadd(key: string, values: { lon: number, lat: number, value: string }[]): void;
  georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: { count: number }): void;
  exec(): Promise<string[]>;
};

export type ICacheClient = {
  set(key: string, value: string): Promise<void>;
  get(key: string): Promise<string | undefined>;
  del(keys: string[]): Promise<void>;
  exists(key: string): Promise<boolean>;
  hget(key: string, field: string): Promise<string | undefined>;
  hmget(key: string, fields: string[]): Promise<(string | undefined)[]>;
  hset(key: string, field: string, value: string): Promise<void>;
  hmset(key: string, values: [string, string][]): Promise<void>;
  hdel(key: string, fields: string[]): Promise<void>;
  zadd(key: string, values: [string, string][]): Promise<void>;
  zremrangebyscore(key: string, minScore: string, inclusiveMaxScore: string): Promise<void>;
  zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): Promise<string[]>;
  zrevrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): Promise<string[]>;
  zrem(key: string, members: string[]): Promise<void>;
  geoadd(key: string, values: { lon: number, lat: number, value: string }[]): Promise<void>;
  georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: { count: number }): Promise<(string | [string, string | [string, string]])[]>;
  multi(): IMultiCacheClient;
};

export type INotificationManager<C extends IUserContext> = {
  dispatchDeckFactories(store: IStore<C>, sub: string, action: string, obj: M.DeckFactory): Promise<void>,
  dispatchDecks(store: IStore<C>, sub: string, action: string, obj: M.Deck): Promise<void>,
  dispatchCommentLists(store: IStore<C>, sub: string, action: string, obj: M.CommentList): Promise<void>,
  dispatchDealerships(store: IStore<C>, sub: string, action: string, obj: M.Dealership): Promise<void>,
  dispatchCarInventories(store: IStore<C>, sub: string, action: string, obj: M.CarInventory): Promise<void>,
  dispatchSellers(store: IStore<C>, sub: string, action: string, obj: M.Seller): Promise<void>,
  dispatchNotes(store: IStore<C>, sub: string, action: string, obj: M.Note): Promise<void>,
  dispatchCarInfos(store: IStore<C>, sub: string, action: string, obj: M.CarInfo): Promise<void>,
};
