import * as Redis from 'redis';

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
}

export default class CacheClient implements ICacheClient {
  private _redisUrl: string;
  private _client: Redis.RedisClient | undefined;

  constructor(redisUrl: string) {
    this._redisUrl = redisUrl;
  }

  init = async (options?: { flushAll: boolean }) => {
    //console.log(`CONNECTING TO REDIS.... "${config('REDIS_URL')}"`);
    const client = Redis.createClient({
      url: this._redisUrl,
      connect_timeout: 5000, // for testing, use retry strategy for production purposes
      /*
      retry_strategy: options => {
        if (options.error && options.error.code === 'ECONNREFUSED') {
          // End reconnecting on a specific error and flush all commands with
          // a individual error
          return new Error('Redis refused the connection');
        } else if (options.total_retry_time > 1000 * 60 * 60) {
          // End reconnecting after a specific timeout and flush all commands
          // with a individual error
          return new Error('Redis retry time exhausted');
        } else if (options.attempt > 10) {
          // End reconnecting with built in error
          return new Error('Redis Attempts exhausted');
        } else {
          // reconnect after
          return Math.min(options.attempt * 100, 3000);
        }
      }
      */
    });
    try {
      //console.log('REDIS CLIENT CREATED...');
      await new Promise((resolve, reject) => {
        client.on('end', err => err ? resolve(err) : reject());
        client.on('ready', () => { resolve(); });
      });
      //console.log('REDIS CONNECTION ESTABILISHED');
      this._client = client;
    } catch (err) {
      //console.error(`FAILED TO CONNECT TO REDIS - ${JSON.stringify(err)}`);
      this._client = undefined;
      throw err;
    }
  };

  uninit = async () => {
    if (this._client != undefined) {
      const client = this._client;
      this._client = undefined;
      return new Promise<void>((resolve, reject) => client.quit(err => err ? reject(err) : resolve()));
    }
  };

  reset = async () => {
    if (process.env.NODE_ENV != 'test') {
      throw new Error('ERROR!! cache reset should only be called in the test environment! ignoring');
    }
    if (this._client === undefined) throw new Error('redis client not initialized');
    const client = this._client;
    return new Promise((resolve, reject) =>
      client.flushall(err => err ? reject(err) : resolve()));
  }

  ensureClient = async (): Promise<Redis.RedisClient> => {
    if (this._client == undefined) {
      await this.init();
      if (this._client == undefined) {
        throw new Error('redis client not initialized');
      }
    }
    return this._client;
  }

  //delete by prefix
  //client.EVAL(`return redis.call('del', 'defaultKey', unpack(redis.call('keys', ARGV[1])))`, 0, 'prefix:*');

  async set(key: string, value: string): Promise<void> {
    //console.info(`REDIS SET: ${key} ${value}`);
    const client = await this.ensureClient();
    try {
      return new Promise<void>((resolve, reject) => client.set(key, value, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async get(key: string): Promise<string | undefined> {
    //console.info(`REDIS GET: ${key}`);
    const client = await this.ensureClient();
    try {
      const ret = await new Promise<string>((resolve, reject) => client.get(key, (err, res) => err ? reject(err) : resolve(res)));
      return ret === null ? undefined : ret;
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async del(keys: string[]): Promise<void> {
    //console.info(`REDIS DEL: ${keys.join(', ')}`);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.DEL(keys, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async expire(key: string, seconds: number): Promise<void> {
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.EXPIRE(key, seconds, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async exists(key: string): Promise<boolean> {
    //console.info(`REDIS EXISTS: ${key}`);
    const client = await this.ensureClient();
    try {
      return await new Promise<boolean>((resolve, reject) => client.EXISTS(key, (err, res) => err ? reject(err) : resolve(res != 0)));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async zadd(key: string, values: [string, string][]): Promise<void> {
    //console.info(`REDIS ZADD: ${key}`);
    if (values.length == 0) return;
    const params = values.reduce((sum, i) => [...sum, ...i], [] as string[]);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.ZADD(key, params, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async hget(key: string, field: string): Promise<string | undefined> {
    //console.info(`MULTI REDIS HGET: ${key}`);
    const client = await this.ensureClient();
    try {
      const ret = await new Promise<string>((resolve, reject) => client.HGET(key, field, (err, res) => err ? reject(err) : resolve(res)));
      return ret === null ? undefined : ret;
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async hmget(key: string, fields: string[]): Promise<(string | undefined)[]> {
    //console.info(`MULTI REDIS HMGET: ${key}`);
    const client = await this.ensureClient();
    try {
      const ret = await new Promise<string[]>((resolve, reject) => client.HMGET(key, fields, (err, res) => err ? reject(err) : resolve(res)));
      return ret.map(r => r === null ? undefined : r);
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async hset(key: string, field: string, value: string): Promise<void> {
    //console.info(`REDIS HSET: ${key}`);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.HSET(key, field, value, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async hmset(key: string, values: [string, string][]): Promise<void> {
    //console.info(`REDIS HMSET: ${key}`);
    const params = values.reduce((sum, i) => [...sum, ...i], [] as string[]);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.HMSET(key, params, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async hdel(key: string, fields: string[]): Promise<void> {
    //console.info(`REDIS HDEL: ${key}`);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) => client.HDEL(key, fields, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async zremrangebyscore(key: string, minScore: string, inclusiveMaxScore: string): Promise<void> {
    //console.info(`REDIS ZREMRNAGEBYSCORE: ${key}`);
    const client = await this.ensureClient();
    try {
      await new Promise<void>((resolve, reject) =>
        client.ZREMRANGEBYSCORE(key, minScore, inclusiveMaxScore, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): Promise<string[]> {
    //console.info(`REDIS ZRANGEBYSCORE: ${key}`);
    const client = await this.ensureClient();
    try {
      if (options == undefined) {
        return await new Promise<string[]>((resolve, reject) => client.ZRANGEBYSCORE(
          key, inclusiveMinScore, inclusiveMaxScore, (err, items) => err ? reject(err) : resolve(items)));
      } else {
        return await new Promise<string[]>((resolve, reject) => client.ZRANGEBYSCORE(
          key, inclusiveMinScore, inclusiveMaxScore, 'LIMIT', options.offset, options.count, (err, items) => err ? reject(err) : resolve(items)));
      }
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async zrevrangebyscore(key: string, inclusiveMaxScore: string, inclusiveMinScore: string, options?: { offset: number, count: number }): Promise<string[]> {
    //console.info(`REDIS ZREVRANGEBYSCORE: ${key}`);
    const client = await this.ensureClient();
    try {
      if (options == undefined) {
        return await new Promise<string[]>((resolve, reject) => client.ZREVRANGEBYSCORE(
          key, inclusiveMaxScore, inclusiveMinScore, (err, items) => err ? reject(err) : resolve(items)));
      } else {
        return await new Promise<string[]>((resolve, reject) => client.ZREVRANGEBYSCORE(
          key, inclusiveMaxScore, inclusiveMinScore, 'LIMIT', options.offset, options.count, (err, items) => err ? reject(err) : resolve(items)));
      }
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async zrem(key: string, members: string[]): Promise<void> {
    const client = await this.ensureClient();
    try {
      return await new Promise<void>((resolve, reject) => client.ZREM(key, members, (err) => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async geoadd(key: string, values: { lon: number, lat: number, value: string }[]): Promise<void> {
    if (values.length == 0) return;
    const client = await this.ensureClient();
    const params = [].concat.apply([], values.map(v => [v.lon, v.lat, v.value])) as (string | number)[];
    try {
      await new Promise<void>((resolve, reject) => client.GEOADD(key, params, err => err ? reject(err) : resolve()));
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  async georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: { count: number }): Promise<(string | [string, string | [string, string]])[]> {
    const client = await this.ensureClient();
    try {
      if (options == undefined) {
        return await new Promise<(string | [string, string | [string, string]])[]>((resolve, reject) => client.GEORADIUS(key, lon, lat, radius, unit, (err, items) => err ? reject(err) : resolve(items)));
      } else {
        return await new Promise<(string | [string, string | [string, string]])[]>((resolve, reject) => client.GEORADIUS(key, lon, lat, radius, unit, 'COUNT', options.count, (err, items) => err ? reject(err) : resolve(items)));
      }
    } catch (err) {
      await this.uninit();
      throw err;
    }
  }
  multi(): IMultiCacheClient {
    const that = this;
    class MultiCacheClient implements IMultiCacheClient {
      fns: ((multi: Redis.Multi) => Redis.Multi)[];
      constructor() {
        this.fns = [];
      }
      set(key: string, value: string): void {
        //console.info(`MULTI REDIS SET: ${key} ${value}`);
        this.fns.push(m => { m.SET(key, value); return m; });
      }
      get(key: string): void {
        //console.info(`MULTI REDIS GET: ${key}`);
        this.fns.push(m => { m.GET(key); return m; });
      }
      del(keys: string[]): void {
        //console.info(`MULTI REDIS DEL: ${keys.join(', ')}`);
        this.fns.push(m => { m.DEL(keys); return m; });
      }
      expire(key: string, seconds: number): void {
        this.fns.push(m => { m.EXPIRE(key, seconds); return m; });
      }
      exists(key: string): void {
        //console.info(`MULTI REDIS EXISTS: ${key}`);
        this.fns.push(m => { m.EXISTS(key); return m; });
      }
      zadd(key: string, values: [string, string][]): void {
        //console.info(`MULTI REDIS ZADD: ${key}`);
        if (values.length == 0) return;
        const params = values.reduce((sum, i) => [...sum, ...i], [] as string[]);
        this.fns.push(m => { m.ZADD(key, params); return m; });
      }
      hget(key: string, field: string): void {
        //console.info(`MULTI REDIS HGET: ${key}`);
        this.fns.push(m => { m.HGET(key, field); return m; });
      }
      hmget(key: string, fields: string[]): void {
        //console.info(`MULTI REDIS HMGET: ${key}`);
        this.fns.push(m => { m.HMGET(key, fields); return m; });
      }
      hset(key: string, field: string, value: string): void {
        //console.info(`MULTI REDIS HSET: ${key}`);
        this.fns.push(m => { m.HSET(key, field, value); return m; });
      }
      hmset(key: string, values: [string, string][]): void {
        //console.info(`MULTI REDIS HMSET: ${key}`);
        const params = values.reduce((sum, i) => [...sum, ...i], [] as string[]);
        this.fns.push(m => { m.HMSET(key, params); return m; });
      }
      hdel(key: string, fields: string[]): void {
        //console.info(`MULTI REDIS HDEL: ${key}`);
        this.fns.push(m => { m.HDEL(key, fields); return m; });
      }
      zremrangebyscore(key: string, minScore: string, inclusiveMaxScore: string): void {
        //console.info(`MULTI REDIS ZREMRNAGEBYSCORE: ${key}`);
        this.fns.push(m => { m.ZREMRANGEBYSCORE(key, minScore, inclusiveMaxScore); return m; });
      }
      zrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): void {
        //console.info(`MULTI REDIS ZRANGEBYSCORE: ${key}`);
        if (options == undefined) {
          this.fns.push(m => { m.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore); return m; });
        } else {
          this.fns.push(m => { m.ZRANGEBYSCORE(key, inclusiveMinScore, inclusiveMaxScore, 'LIMIT', options.offset, options.count); return m; });
        }
      }
      zrevrangebyscore(key: string, inclusiveMinScore: string, inclusiveMaxScore: string, options?: { offset: number, count: number }): void {
        //console.info(`MULTI REDIS ZREVRANGEBYSCORE: ${key}`);
        if (options == undefined) {
          this.fns.push(m => { m.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore); return m; });
        } else {
          this.fns.push(m => { m.ZREVRANGEBYSCORE(key, inclusiveMaxScore, inclusiveMinScore, 'LIMIT', options.offset, options.count); return m; });
        }
      }
      zrem(key: string, members: string[]): void {
        this.fns.push(m => { m.ZREM(key, members); return m; });
      }
      geoadd(key: string, values: { lon: number, lat: number, value: string }[]): void {
        if (values.length == 0) return;
        const params = [].concat.apply([], values.map(v => [v.lon, v.lat, v.value])) as (string | number)[];
        this.fns.push(m => { m.GEOADD(key, params); return m; });
      }
      georadius(key: string, lon: number, lat: number, radius: number, unit: 'm' | 'km' | 'ft' | 'mi', options?: { count: number }): void {
        if (options == undefined) {
          this.fns.push(m => { m.GEORADIUS(key, lon, lat, radius, unit); return m; });
        } else {
          this.fns.push(m => { m.GEORADIUS(key, lon, lat, radius, unit, 'COUNT', options.count); return m; });
        }
      }
      async exec(): Promise<string[]> {
        let multi = (await that.ensureClient()).multi();
        this.fns.forEach(fn => { multi = fn(multi); });
        return new Promise<string[]>((resolve, reject) => {
          multi.exec((err, replies) => {
            if (err) {
              reject(err);
            } else {
              resolve(replies.map(r => r.toString()));
            }
          });
        });
      }
    }
    return new MultiCacheClient();
  }
};
