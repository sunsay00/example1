import * as Redis from 'redis';
export declare type IMultiCacheClient = {
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
    zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: {
        offset: number;
        count: number;
    }): void;
    zrevrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: {
        offset: number;
        count: number;
    }): void;
    zrem(key: string, members: string[]): void;
    geoadd(key: string, values: {
        lon: number;
        lat: number;
        value: string;
    }[]): void;
    georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: {
        count: number;
    }): void;
    exec(): Promise<string[]>;
};
export declare type ICacheClient = {
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
    zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: {
        offset: number;
        count: number;
    }): Promise<string[]>;
    zrevrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: {
        offset: number;
        count: number;
    }): Promise<string[]>;
    zrem(key: string, members: string[]): Promise<void>;
    geoadd(key: string, values: {
        lon: number;
        lat: number;
        value: string;
    }[]): Promise<void>;
    georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: {
        count: number;
    }): Promise<(string | [string, string | [string, string]])[]>;
    multi(): IMultiCacheClient;
};
export default class CacheClient implements ICacheClient {
    private _redisUrl;
    private _client;
    constructor(redisUrl: string);
    init: (options?: {
        flushAll: boolean;
    } | undefined) => Promise<void>;
    uninit: () => Promise<void>;
    reset: () => Promise<unknown>;
    ensureClient: () => Promise<Redis.RedisClient>;
    set(key: string, value: string): Promise<void>;
    get(key: string): Promise<string | undefined>;
    del(keys: string[]): Promise<void>;
    expire(key: string, seconds: number): Promise<void>;
    exists(key: string): Promise<boolean>;
    zadd(key: string, values: [string, string][]): Promise<void>;
    hget(key: string, field: string): Promise<string | undefined>;
    hmget(key: string, fields: string[]): Promise<(string | undefined)[]>;
    hset(key: string, field: string, value: string): Promise<void>;
    hmset(key: string, values: [string, string][]): Promise<void>;
    hdel(key: string, fields: string[]): Promise<void>;
    zremrangebyscore(key: string, minScore: string, inclusiveMaxScore: string): Promise<void>;
    zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: {
        offset: number;
        count: number;
    }): Promise<string[]>;
    zrevrangebyscore(key: string, inclusiveMaxScore: string, inclusiveMinScore: string, options?: {
        offset: number;
        count: number;
    }): Promise<string[]>;
    zrem(key: string, members: string[]): Promise<void>;
    geoadd(key: string, values: {
        lon: number;
        lat: number;
        value: string;
    }[]): Promise<void>;
    georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: {
        count: number;
    }): Promise<(string | [string, string | [string, string]])[]>;
    multi(): IMultiCacheClient;
}
//# sourceMappingURL=index.d.ts.map