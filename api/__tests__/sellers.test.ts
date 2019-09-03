import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext } from '@inf/cf-gen';
import { vars } from '../src/vars';

const config = verifyVars({
  STAGE: process.env.STAGE,
  AWS_REGION: process.env.AWS_REGION
});

const db = new RDSDBClient(vars.DB_TEST_URL);
const cache = undefined; // new CacheClient(config.redisUrl),

const resolver = createDefaultResolver({
  stage: config.STAGE,
  region: config.AWS_REGION,
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache,
});

describe('sellers', () => {
  beforeAll(async () => db.init());
  afterAll(async () => db.deinit());

  it('should fail for a user to be more than one seller', async () => {
    const ctxs1: IUserContext = { sub: 'subsellerdup1', groups: [] };
    const s1 = await fix.sellersCreateOrFindBySub(resolver, ctxs1, '10000002', 'en-US', 'name1@chat.com', 'name1', 0, 'bio', true);
    try {
      await fix.sellersCreateOrFindBySub(resolver, ctxs1, '1000001', 'en-US', 'name2@chat.com', 'name2', 0, 'bio', true);
      expect(false).toBeFalsy();
    } catch (err) {
      expect(err).toMatchSnapshot();
    } finally {
      await fix.sellersRemove(resolver, ctxs1, s1.id);
    }
  });

  it('should upsert a seller', async () => {
    const ctx: IUserContext = { sub: 'upsertsub1', groups: [] };
    const s1 = await fix.sellersCreateOrFindBySub(resolver, ctx, '100', 'en-US', 'name1@chat.com', 'name1', 0, 'bio', true);
    expect(s1.dealership).toBeNull();
    const s2 = await fix.sellersCreateOrFindBySub(resolver, ctx, '101', 'en-US', 'name2@chat.com', 'name2', 1, 'bio', true);
    expect(s2.dealership).toBeNull();
    expect(s1.id).toEqual(s2.id);
    expect(s1.sub).toEqual(ctx.sub);
    expect(s1.name).toEqual('name1');
    expect(s2.sub).toEqual(ctx.sub);
    expect(s2.name).toEqual('name1');
    await fix.sellersRemove(resolver, ctx, s1.id);
  });

  it('should have associated dealerships', async () => {
    const ctxd: IUserContext = { sub: 'associateddealer1', groups: [] };
    const ctxs: IUserContext = { sub: 'associateddealer2', groups: [] };
    const d1 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd, 'us-east1', 'dealer@chat.com', 'dealername', ['make1', 'make2'], 'my address');
    const s1 = await fix.sellersCreateOrFindBySub(resolver, ctxs, d1.id, 'en-US', 'name1@chat.com', 'name1', 0, 'bio', true);
    expect(s1.dealership).toBeDefined();
    const s2 = await fix.sellersCreateOrFindBySub(resolver, ctxs, '0', 'en-US', 'name2@chat.com', 'name2', 1, 'bio', true);
    expect(s2.dealership).toBeDefined();
    if (s1.dealership && s2.dealership) {
      expect(s1.dealership.id).toEqual(s2.dealership.id);
    }
  });

  it('should find sellers based on location', async () => {
    //const empireStateBuildingLoc = { lon: -73.9856644, lat: 40.7484405 };
    //const nycLoc = { lon: -74.0059728, lat: 40.7127753 };
    const ctxd1: IUserContext = { sub: 'subd1', groups: [] };
    const ctxd2: IUserContext = { sub: 'subd2', groups: [] };
    const ctxd3: IUserContext = { sub: 'subd3', groups: [] };
    const d1 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd1, 'en-US', 'dealership1@chat.com', 'dealership1', ['make1'], 'address1', { lon: 0.1, lat: 0.1 }, 'title1');
    const d2 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd2, 'en-US', 'dealership2@chat.com', 'dealership2', ['make1'], 'address2', { lon: 0.1, lat: 0.2 }, 'title2');
    const d3 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd3, 'en-US', 'dealersgip3@chat.com', 'dealership3', ['make1'], 'address3', { lon: 0.1, lat: 0.3 }, 'title3');

    const ctxs1: IUserContext = { sub: 'subs1', groups: [] };
    const ctxs2: IUserContext = { sub: 'subs2', groups: [] };
    const ctxs3: IUserContext = { sub: 'subs3', groups: [] };
    const s1 = await fix.sellersCreateOrFindBySub(resolver, ctxs1, d1.id, 'en-US', 'seller1@chat.com', 'seller1', 1, 'bio1', true);
    const s2 = await fix.sellersCreateOrFindBySub(resolver, ctxs2, d2.id, 'en-US', 'seller2@chat.com', 'seller2', 2, 'bio2', true);
    const s3 = await fix.sellersCreateOrFindBySub(resolver, ctxs3, d3.id, 'en-US', 'seller3@chat.com', 'seller3', 3, 'bio3', true);

    const ctxb1: IUserContext = { sub: 'subb1', groups: [] };
    const results = await fix.sellersSearch(resolver, ctxb1, { lon: 0.1, lat: 0.1 }, 10.0, 'make1');
    expect(results.items).toHaveLength(2);
    const items = results.items.sort((a, b) => a.id.localeCompare(b.id));
    const ss = [s1, s2].sort((a, b) => a.id.localeCompare(b.id));
    expect(items[0].id).toEqual(ss[0].id);
    expect(items[1].id).toEqual(ss[1].id);
    expect(items[0].dealership).toBeDefined();
    expect(items[1].dealership).toBeDefined();

    await fix.sellersRemove(resolver, ctxs3, s3.id);
    await fix.sellersRemove(resolver, ctxs2, s2.id);
    await fix.sellersRemove(resolver, ctxs1, s1.id);

    await fix.dealershipsRemove(resolver, ctxd3, d3.id);
    await fix.dealershipsRemove(resolver, ctxd2, d2.id);
    await fix.dealershipsRemove(resolver, ctxd1, d1.id);
  });

});

