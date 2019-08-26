import { ConfigRecord } from '@inf/vars/configure';

export const Config = (params: { rootEnv: string }): ConfigRecord => ({
  type: 'shell',
  name: 'cf-awsinfo',
  command: `${__dirname}/make`,
  dependsOn: [params.rootEnv],
  args: ['configure'],
});
