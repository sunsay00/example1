import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, Deck } from '@inf/cf-gen';
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

describe('customtypes', () => {
  let _user: IUserContext;
  beforeAll(() => {
    _user = {
      sub: '1',
      groups: [],
    };
  });

  describe('datetime', () => {
    it('should succeed with good input', async () => {
      const query = `query ($dateTime: DateTime) {
        testDateTimeType (dateTime: $dateTime) {
          dateTime
        }
      }`;
      const json = await resolver.resolve({}, query, {
        dateTime: new Date(0).toJSON(),
      }, _user);
      expect(json).toMatchObject({ data: expect.any(Object) });
      expect(json).toMatchSnapshot();
    });

    it('should succeed with good inline arguments', async () => {
      const query = `query  {
        testDateTimeType (dateTime: "${new Date(0).toJSON()}") {
          dateTime
        }
      }`;
      const json = await resolver.resolve({}, query, {}, _user);
      expect(json).toMatchObject({ data: expect.any(Object) });
      expect(json).toMatchSnapshot();
    });
  });

  describe('email', () => {
    it('should succeed with good input', async () => {
      const query = `query ($email:  Email) {
        testEmailType (email: $email) {
          email
        }
      }`;
      const json = await resolver.resolve({}, query, {
        email: 'a@b.co',
      }, _user);
      expect(json).toMatchObject({ data: expect.any(Object) });
      expect(json).toMatchSnapshot();
    });

    it('should fail when invoked with bad input', async () => {
      const query = `query ($email:  Email) {
        testEmailType (email: $email) {
          email
        }
      }`;
      const json = await resolver.resolve({}, query, {
        email: 'w38957fgj',
      }, _user);
      expect(json).toMatchObject({ errors: expect.any(Array) });
      expect(json).toMatchSnapshot();
    });
  });

})