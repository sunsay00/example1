import { createConfigRecord } from '@inf/vars/configure';

export const Config = (params: { rootEnv: string }) => createConfigRecord({
  type: 'shell',
  name: 'cf-awsinfo',
  command: `${__dirname}/make`,
  dependsOn: [params.rootEnv],
  args: ['configure'],
});
