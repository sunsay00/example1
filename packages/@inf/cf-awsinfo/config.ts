import { ConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: { dependsOn?: string[] }): ConfigRecord => ({
  type: 'shell',
  name: 'cf-awsinfo',
  command: `${__dirname}/make`,
  dependsOn: inputs.dependsOn,
  args: ['configure']
});
