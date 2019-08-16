import { ConfigRecord } from '@inf/configure';

export const Config = (): ConfigRecord => ({
  type: 'shell',
  key: 'GEN',
  cwd: __dirname,
  command: 'gen',
  args: [],
});
