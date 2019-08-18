import { ConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: { dependsOn?: string[] }): ConfigRecord => ({
  type: 'shell',
  name: 'cf-awsinfo',
  cwd: __dirname,
  command: 'make',
  dependsOn: inputs.dependsOn,
  args: ['configure']
});
