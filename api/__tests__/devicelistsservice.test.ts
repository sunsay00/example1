import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, User } from '@inf/cf-gen';
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

describe('devicelists', () => {
  const ctx: IUserContext = { sub: 'deviceuser', groups: [] };
  let user: User;

  beforeAll(async () => {
    await db.init();

    user = await fix.backendCreateUser(resolver.store(), ctx, "user", "avataruri.jpg", "en", "us-east-1");
    expect(user).toBeDefined();
  });
  afterAll(async () => {
    expect(await fix.backendRemoveUser(resolver.store(), ctx, user.id)).toBeDefined();

    await db.deinit();
  });

  it.skip('should register a deviceid', async () => {
    const device = await fix.deviceListsRegister(resolver, ctx, '1234567890', 'my device details', '1234567890@chat.com');
    expect(device).toBeDefined();
    if (device == undefined) throw new Error('failed to register device');

    const { createdAt, ...deviceRest } = device;
    expect(deviceRest).toMatchSnapshot();

    const devices = await fix.deviceListsMyDevices(resolver, ctx, '1234567890@chat.com');
    expect(devices.items).toHaveLength(1);
    expect(devices.cursor).toBeNull();
    const devicesRest = devices.items.map(d => { const { createdAt, ...rest } = d; return rest; });
    expect(devicesRest).toMatchSnapshot();

    expect(await fix.deviceListsUnregister(resolver, ctx, device.token, '1234567890@chat.com')).toBeDefined();
    expect((await fix.deviceListsMyDevices(resolver, ctx, '1234567890@chat.com')).items).toHaveLength(0);
  });

  it.skip('should register two deviceids', async () => {
    const device1 = await fix.deviceListsRegister(resolver, ctx, '0987654321', 'my device1 details', '0987654321@chat.com');
    const device2 = await fix.deviceListsRegister(resolver, ctx, '1234567890', 'my device2 details', '0987654321@chat.com');
    if (device1 == undefined || device2 == undefined) throw new Error('invalid device');

    expect((await fix.deviceListsMyDevices(resolver, ctx, '0987654321@chat.com')).items).toHaveLength(2);

    expect(await fix.deviceListsUnregister(resolver, ctx, device1.token, '0987654321@chat.com')).toBeDefined();
    expect((await fix.deviceListsMyDevices(resolver, ctx, '0987654321@chat.com')).items).toHaveLength(1);

    expect(await fix.deviceListsUnregister(resolver, ctx, device2.token, '0987654321@chat.com')).toBeDefined();
    expect((await fix.deviceListsMyDevices(resolver, ctx, '0987654321@chat.com')).items).toHaveLength(0);

  });

});
