import { ConfigRecord } from '@inf/vars/configure';

export const Config = (): ConfigRecord => ({
  type: 'shell',
  name: 'cf-gen',
  cwd: __dirname,
  command: 'gen',
  args: [],
});
