import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix } from '@inf/cf-gen';

const db = new RDSDBClient('');

const resolver = createDefaultResolver({
  stage: 'dev',
  region: 'us-east-1',
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache: undefined, // new CacheClient(config.redisUrl),
});

describe('carinfos', () => {
  beforeAll(async () => await db.init());
  afterAll(async () => await db.deinit());

  it('should succeed', () => {
    expect(true).toBe(true);
  });

  it('should get a unique list of colors', async () => {
    const ctx = { sub: 'searcher', username: 'searcher', groups: [] };
    const c1 = await fix.carInfosCreate(resolver, ctx, '1900', 'make1', 'model1', 'color1');
    const c2 = await fix.carInfosCreate(resolver, ctx, '1901', 'make2', 'model2', 'color1');
    const c3 = await fix.carInfosCreate(resolver, ctx, '1902', 'make3', 'model3', 'color2');
    const colors = await fix.carInfosUniqueColors(resolver, ctx, []);
    expect(colors.items).toHaveLength(2);
    const sorted = colors.items.map(c => c).sort((a, b) => a.localeCompare(b));
    expect(sorted[0]).toEqual('color1');
    expect(sorted[1]).toEqual('color2');
    await fix.carInfosRemove(resolver, ctx, c1.id);
    await fix.carInfosRemove(resolver, ctx, c2.id);
    await fix.carInfosRemove(resolver, ctx, c3.id);
  });
});