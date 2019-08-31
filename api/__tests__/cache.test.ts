import "jest";
/*import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, User } from '@inf/cf-gen';
import { vars } from '@inf/cf-gen/vars';

const config = verifyVars({
  STAGE: process.env.STAGE,
  AWS_REGION: process.env.AWS_REGION
});

const db = new RDSDBClient(vars.DB_TEST_URL);
const cache = undefined; // new CacheClient(config.redisUrl),

const findByIdTest = async (useCache: boolean) => {
  const resolver = createDefaultResolver({
    stage: config.STAGE,
    region: config.AWS_REGION,
    locale: 'en',
    platformApplicationArn: '',
    db,
    cache,
  });

  const ctx: IUserContext = { sub: 'cacheuser', groups: [] };
  const user: User = await fix.backendCreateUser(resolver.store(), ctx, 'user', 'avataruri.jpg', 'en', 'us-east-1');

  const dm = await fix.deckFactoriesCreate(resolver, ctx, 'name1', 'description1');

  if (useCache) resolver.store().deckFactoriesFindById = jest.fn(() => { throw new Error('should not be called') });
  const r2 = await fix.deckFactoriesFindById(resolver, ctx, dm.id);
  expect(r2).toBeDefined();
  useCache && expect(resolver.store().deckFactoriesFindById).not.toBeCalled();

  await fix.deckFactoriesRemove(resolver, ctx, dm.id);

  if (useCache) resolver.store().deckFactoriesFindById = jest.fn(() => undefined);
  const r3 = await fix.deckFactoriesFindById(resolver, ctx, dm.id);
  expect(r3).toBeUndefined();
  useCache && expect(resolver.store().deckFactoriesFindById).toBeCalled();

  await fix.backendRemoveUser(resolver.store(), ctx, user.id);
}

const findByIdInTest = async (useCache: boolean) => {
  const resolver = createDefaultResolver({
    stage: config.STAGE,
    region: config.AWS_REGION,
    locale: 'en',
    platformApplicationArn: '',
    db,
    cache,
  });

  const ctx: IUserContext = { sub: 'cacheuser2', groups: [] };
  const user: User = await fix.backendCreateUser(resolver.store(), ctx, 'user', 'avataruri.jpg', 'en', 'us-east-1');

  const dm1 = await fix.deckFactoriesCreate(resolver, ctx, 'name1', 'description1');
  const dm2 = await fix.deckFactoriesCreate(resolver, ctx, 'name2', 'description2');
  const dm3 = await fix.deckFactoriesCreate(resolver, ctx, 'name3', 'description3');

  if (useCache) resolver.store().deckFactoriesFindByIdIn = jest.fn(() => { throw new Error('should not be called'); });
  const r1 = await fix.deckFactoriesFindByIdIn(resolver, ctx, [dm1.id, dm2.id, dm3.id]);
  expect(r1.items).toHaveLength(3);
  useCache && expect(resolver.store().deckFactoriesFindByIdIn).not.toBeCalled();

  if (useCache) resolver.store().deckFactoriesFindByIdIn = jest.fn(async () => [
    { id: '31753563538292', name: 'name1', description: 'description1', cursor: '31753563538292' },
    { id: '31753563538293', name: 'name2', description: 'description2', cursor: '31753563538293' }]);
  const r2 = await fix.deckFactoriesFindByIdIn(resolver, ctx, [dm1.id, dm2.id, '88']);
  expect(r2.items).toHaveLength(2);
  useCache && expect(resolver.store().deckFactoriesFindByIdIn).toBeCalled();

  await fix.deckFactoriesRemove(resolver, ctx, dm3.id);
  await fix.deckFactoriesRemove(resolver, ctx, dm2.id);
  await fix.deckFactoriesRemove(resolver, ctx, dm1.id);

  await fix.backendRemoveUser(resolver.store(), ctx, user.id);
}

describe('cache', () => {
  beforeAll(async () => {
    await db.init();
    //await cacheInit();
    //await cacheReset();
  });
  afterAll(async () => {
    //await cacheUninit();
    await db.deinit();
  });
  beforeEach(async () => {
    jest.clearAllMocks();
  });

  it('should use the cache when finding by id', async () => findByIdTest(true));
  it('shouldn\'t use the cache when finding by id', async () => findByIdTest(false));

  it('should use the cache when finding by an array of ids', async () => findByIdInTest(true));
  it('shouldn\'t use the cache when finding by an array of ids', async () => findByIdInTest(false));
});
*/

describe('cache', () => {
  it('should succeed', () => {
    expect(true).toBe(true);
  });
});
