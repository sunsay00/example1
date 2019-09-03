import "jest";
import { verifyVars } from '@inf/common';
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

const resolver = createDefaultResolver({
  stage: config.STAGE,
  region: config.AWS_REGION,
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache,
});

describe('pagination', () => {
  const ctx: IUserContext = { sub: 'pagination-tester-1', groups: [] };
  let user: User;
  beforeAll(async () => {
    await db.init();
    user = await fix.backendCreateUser(resolver.store(), ctx, "user", "avataruri.jpg", "en", "us-east-1");
  });
  afterAll(async () => {
    await fix.backendRemoveUser(resolver.store(), ctx, user.id);
    await db.deinit();
  });

  it('should paginate', async () => {
    const N = 3;
    const deckIds: string[] = [];
    for (let i = 0; i < N; ++i) { deckIds.push((await fix.decksCreate(resolver, ctx, `${i}`, [], undefined, `${i}`)).id); }

    const decks1 = await fix.decksFind(resolver, ctx);
    expect(decks1.items).toHaveLength(3);
    expect(decks1.cursor).toBeNull();
    const decks3 = await fix.decksFind(resolver, ctx, undefined, 2);
    expect(decks3.items).toHaveLength(2);
    expect(decks3.cursor).toEqual(decks3.items[1].id);
    const decks4 = await fix.decksFind(resolver, ctx, decks3.cursor, 2);
    expect(decks4.items).toHaveLength(1);
    expect(decks4.cursor).toBeNull();

    for (let i = 0; i < N; ++i)
      await fix.decksRemove(resolver, ctx, deckIds[i]);
  });
});

