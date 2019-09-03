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

describe('systems', () => {
  beforeAll(async () => {
    await db.init();
    //await cacheInit();
  });

  afterAll(async () => {
    //await cacheUninit();
    await db.deinit();
  });

  it.skip('should get a chat token', async () => {
    const ctx: IUserContext = { sub: 'system-sub-001', groups: [] };
    const result = await fix.systemsGetChatToken(resolver, ctx, 'myidentity', 'none');
    expect(result).toBeDefined();
    expect(result.length).toBeGreaterThan(10);
  });
});
