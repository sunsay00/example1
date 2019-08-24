import { IStore, IUserContext, ICacheClient, IDBClient } from '../types';

export const runCacheStoreSmokeTests = async <C extends IUserContext>(store: IStore<C>, client: ICacheClient) => {
  await client.del(['check:test-key']);
  const value = await client.get('check:test-key');
  if (value != undefined) throw new Error('redis get key smoketest failed');
  await client.set('check:test-key', '123');
  if ('123' != await client.get('check:test-key')) throw new Error('redis get key smoketest failed (2)');

  return await store.runSmokeTests();
};

export const runRDSDBStoreSmokeTests = async (client: IDBClient) => {
  const ret = await client.query('select 1 + 1 as sum;', []);
  if (ret.rowCount != 1 && ret.rows[0]['sum'] != 2) throw new Error('rds smoketest failed');
  return 'success';
};