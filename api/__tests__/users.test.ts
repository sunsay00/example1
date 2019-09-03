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

describe('users', () => {
  beforeAll(async () => {
    await db.init();
    //await cacheInit();

  });
  afterAll(async () => {
    await db.deinit();
    //await cacheUninit();
  });

  it('should create and remove a user using only backend', async () => {
    const ctx: IUserContext = { sub: 'mysub1', groups: [] };
    const { id, ...user } = await fix.backendCreateUser(resolver.store(), ctx, 'testname', 'http://http://placehold.it/24x24', 'en', 'us-east-1');
    expect(user).toMatchSnapshot();
    const foundResult = await fix.backendFindUserById(resolver.store(), ctx, user.sub, id);
    expect(foundResult).toBeDefined();
    const result = await fix.backendRemoveUser(resolver.store(), ctx, id);
    expect({ sub: result.sub, fields: result.fields }).toMatchSnapshot();
    const foundResult2 = await fix.backendFindUserById(resolver.store(), ctx, user.sub, id);
    expect(foundResult2).toBeUndefined();
  });

  it('should make deck returns correct user fields', async () => {
    const ctx: IUserContext = { sub: 'mysub', groups: [] };
    const deck = await fix.createDeck(resolver, ctx, '0', 'mytitle', 'mydescription');
    const backendUser = await fix.backendCreateUser(resolver.store(), ctx, 'testname', 'http://http://placehold.it/24x24', 'en', 'us-east-1');

    const found1 = await fix.findDeckById(resolver, ctx, deck.id);
    if (found1 == undefined) throw new Error('failed to find deck');
    const { id: id1, ...deck1 } = found1;
    expect(deck1.user).not.toBeNull();
    expect(deck1).toMatchSnapshot();

    const updated = await resolver.store().usersUpdate(ctx, backendUser.id, { name: 'newtestname', avatarUri: 'http://http://placehold.it/48x48' });
    expect(updated).toBeDefined();
    const found2 = await fix.findDeckById(resolver, ctx, deck.id);
    if (found2 == undefined) throw new Error('failed to find deck');
    const { id: id2, ...deck2 } = found2;
    expect(deck2.user).not.toBeNull();
    expect(deck2).toMatchSnapshot();

    await fix.backendRemoveUser(resolver.store(), ctx, backendUser.id);

    const found3 = await fix.findDeckById(resolver, ctx, deck.id);
    if (found3 == undefined) throw new Error('failed to find deck');
    expect(found3.user).toBeNull();

    const result = await fix.removeDeck(resolver, ctx, deck);
    expect(result).toBeDefined();
  });

  it('should get me', async () => {
    const ctx: IUserContext = { sub: 'me', groups: [] };
    const backendUser = await fix.backendCreateUser(resolver.store(), ctx, 'testname', 'http://placehold.it/24x24', 'en', 'us-east-1');
    const { fields: result1 } = await fix.usersMe(resolver, ctx);
    expect(result1.name).toEqual('testname');
    expect(result1.avatarUri).toEqual('http://placehold.it/24x24');
    const updateResult = await fix.usersUpdateMe(resolver, ctx, { name: 'changedname', avatarUri: 'http://placehold.it/48x48' });
    expect(updateResult).toBeTruthy();
    const { fields: result2 } = await fix.usersMe(resolver, ctx);
    expect(result2.name).toEqual('changedname');
    expect(result2.avatarUri).toEqual('http://placehold.it/48x48');
    await fix.backendRemoveUser(resolver.store(), ctx, backendUser.id);
  });

});