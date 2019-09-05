import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext } from '@inf/cf-gen';
import { vars } from '../src/_vars';

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

  it('should fail for a user to have two dealerships', async () => {
    const ctxd1: IUserContext = { sub: 'subdup1', groups: [] };
    const d1 = await fix.dealershipsCreateOrFindBySub(resolver, ctxd1, 'en-US', 'dealership1@chat.com', 'dealership1', ['make1', 'make2'], 'address1', { lon: 0.1, lat: 0.1 });
    try {
      await fix.dealershipsCreateOrFindBySub(resolver, ctxd1, 'en-US', 'dealership2@chat.com', 'dealerships2', ['make2', 'make3'], 'address2', { lon: 0.1, lat: 0.1 });
      expect(false).toBeFalsy();
    } catch (err) {
      expect(err).toMatchSnapshot();
    } finally {
      await fix.dealershipsRemove(resolver, ctxd1, d1.id);
    }
  });
});

