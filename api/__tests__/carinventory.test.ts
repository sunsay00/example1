import "jest";
import { uniq } from 'lodash';
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, Dealership, Seller, CarInventory } from '@inf/cf-gen';
import { vars } from '@inf/cf-gen/vars';

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

describe('carinventory', () => {
  const ctx: IUserContext = { sub: 'admin', groups: [] };
  const ctxd1: IUserContext = { sub: 'dealer-sub1', groups: [] };
  const ctxd2: IUserContext = { sub: 'dealer-sub2', groups: [] };
  const ctxd3: IUserContext = { sub: 'dealer-sub3', groups: [] };
  const ctxs1: IUserContext = { sub: 'seller-sub1', groups: [] };
  const ctxs2: IUserContext = { sub: 'seller-sub2', groups: [] };
  const ctxs3: IUserContext = { sub: 'seller-sub3', groups: [] };
  let d1: Dealership;
  let d2: Dealership;
  let d3: Dealership;
  let s1: Seller;
  let s2: Seller;
  let s3: Seller;
  let inventory = [] as CarInventory[];

  beforeAll(async () => {
    await db.init();
    d1 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd1, 'en-US', 'dealership1@chat.com', 'dealership1', ['make1', 'make2'], 'address1', { lon: 30.1, lat: 0.1 });
    d2 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd2, 'en-US', 'dealership2@chat.com', 'dealership2', ['make3'], 'address2', { lon: 30.2, lat: 0.2 });
    d3 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd3, 'en-US', 'dealership3@chat.com', 'dealership3', ['make4'], 'address3', { lon: 30.3, lat: 0.3 });
    s1 = await fix.sellersCreateOrFindBySub(resolver, ctxs1, d1.id, 'en-US', 'seller1@chat.com', 'seller1', 1, 'bio1', true);
    s2 = await fix.sellersCreateOrFindBySub(resolver, ctxs2, d2.id, 'en-US', 'seller2@chat.com', 'seller2', 2, 'bio2', true);
    s3 = await fix.sellersCreateOrFindBySub(resolver, ctxs3, d3.id, 'en-US', 'seller3@chat.com', 'seller3', 3, 'bio3', true);
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '493833', 'make1', 'Dealer 1', 47364, 'F12berlinetta', '120,000', '130,000', 'Black', 'Black', 'Stock', 493833));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '236589', 'make3', 'Dealer 2', 59687, '488 GTB', '120,000', '130,000', 'Red', 'Black', 'Stock', 236589));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '696587', 'make2', 'Dealer 1', 36529, '488 GTB', '124,000', '140,000', 'Red', 'White', 'Stock', 696587));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '899853', 'make4', 'Dealer 3', 32568, 'GTC4Lusso', '124,000', '140,000', 'Black', 'White', 'Stock', 899853));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '273462', 'make1', 'Dealer 1', 96358, 'Ghibli', '154,000', '170,000', 'Black', 'White', 'Stock', 273462));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '365489', 'make3', 'Dealer 2', 32659, 'Gran Turismo', '124,000', '140,000', 'Black', 'Black', 'Stock', 365489));
    inventory.push(await fix.carInventoriesCreate(resolver, ctx, '698657', 'make1', 'Dealer 1', 32698, 'Gran Turismo', '124,000', '140,000', 'Black', 'White', 'Stock', 698657));
  });

  afterAll(async () => {
    for (let i = 0; i < inventory.length; ++i) await fix.carInventoriesRemove(resolver, ctx, inventory[i].id);
    await fix.sellersRemove(resolver, ctxs2, s2.id);
    await fix.sellersRemove(resolver, ctxs3, s3.id);
    await fix.sellersRemove(resolver, ctxs1, s1.id);
    await fix.dealershipsRemove(resolver, ctxd3, d3.id);
    await fix.dealershipsRemove(resolver, ctxd2, d2.id);
    await fix.dealershipsRemove(resolver, ctxd1, d1.id);
    await db.deinit();
  });

  it('should find unique selection sets of models and exteriorColors', async () => {
    const dd1 = await fix.dealershipsFindById(resolver, ctxs1, d1.id);
    if (dd1 == undefined) throw new Error('dealership1 not found');
    const models1 = await fix.carInventoriesModels(resolver, ctxs1, dd1.makes);
    const m1 = models1.items.map(m => { expect(dd1.makes).toContain(m.make); return m.model; }).sort((a, b) => a.localeCompare(b));
    expect(m1).toHaveLength(4);
    expect(m1).toHaveLength(uniq(m1).length);

    const d1c0 = await fix.carInventoriesUniqueColors(resolver, ctxs1, dd1.makes, undefined);
    expect(d1c0.items.sort((a, b) => a.localeCompare(b))).toMatchObject(['Black', 'Red']);
    const d1c1 = await fix.carInventoriesUniqueColors(resolver, ctxs1, dd1.makes, m1[0]);
    expect(d1c1.items).toMatchObject(['Red']);
    const d1c2 = await fix.carInventoriesUniqueColors(resolver, ctxs1, dd1.makes, m1[1]);
    expect(d1c2.items).toMatchObject(['Black']);
    const d1c3 = await fix.carInventoriesUniqueColors(resolver, ctxs1, dd1.makes, m1[2]);
    expect(d1c3.items).toMatchObject(['Black']);
    const d1c4 = await fix.carInventoriesUniqueColors(resolver, ctxs1, dd1.makes, m1[3]);
    expect(d1c4.items).toMatchObject(['Black']);

    const dd2 = await fix.dealershipsFindById(resolver, ctxs2, d2.id);
    if (dd2 == undefined) throw new Error('dealership2 not found');
    const models2 = await fix.carInventoriesUniqueModels(resolver, ctxs2, dd2.makes);
    //const m2 = models2.items;
    expect(models2.items).toHaveLength(2);
    expect(models2.items).toHaveLength(uniq(models2.items).length);

    const dd3 = await fix.dealershipsFindById(resolver, ctxs3, d3.id);
    if (dd3 == undefined) throw new Error('dealership3 not found');
    const models3 = await fix.carInventoriesModels(resolver, ctxs3, dd3.makes);
    expect(models3.items).toHaveLength(1);
    expect(models3.items).toHaveLength(uniq(models3.items).length);
  });

  it('should verify that sellers return proper search results of their respective dealerships', async () => {
    const dd1 = await fix.dealershipsFindById(resolver, ctxs1, d1.id);
    if (dd1 == undefined) throw new Error('dealership1 not found');
    const result1 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes);
    result1.items.map(r => expect(dd1.makes).toContain(r.make));
    expect(result1.items).toHaveLength(4);

    const d1r1 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes, undefined, 'Red');
    d1r1.items.map(r => expect(r.exteriorColor).toBe('Red'));
    expect(d1r1.items).toHaveLength(1);
    const d1r2 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes, undefined, 'Black');
    d1r2.items.map(r => expect(r.exteriorColor).toBe('Black'));
    expect(d1r2.items).toHaveLength(3);
    const d1r3 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes, 'F12berlinetta');
    d1r3.items.map(r => expect(r.model).toEqual('F12berlinetta'));
    expect(d1r3.items).toHaveLength(1);
    const d1r4 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes, 'F12berlinetta', 'Black');
    d1r4.items.map(r => {
      expect(r.model).toEqual('F12berlinetta');
      expect(r.exteriorColor).toEqual('Black');
    });
    expect(d1r4.items).toHaveLength(1);
    const d1r5 = await fix.carInventoriesSearch(resolver, ctx, dd1.makes, 'F12berlinetta', 'Red');
    expect(d1r5.items).toHaveLength(0);

    const dd2 = await fix.dealershipsFindById(resolver, ctxs2, d2.id);
    if (dd2 == undefined) throw new Error('dealership2 not found');
    const result2 = await fix.carInventoriesSearch(resolver, ctx, dd2.makes);
    result2.items.map(r => expect(dd2.makes).toContain(r.make));
    expect(result2.items).toHaveLength(2);

    const dd3 = await fix.dealershipsFindById(resolver, ctxs3, d3.id);
    if (dd3 == undefined) throw new Error('dealership3 not found');
    const result3 = await fix.carInventoriesSearch(resolver, ctx, dd3.makes);
    result3.items.map(r => expect(dd3.makes).toContain(r.make));
    expect(result3.items).toHaveLength(1);
  });
});


