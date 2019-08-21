import { ConfigRecord } from '@inf/vars/configure';

export type Inputs = {};

export const Config = (inputs: Inputs): ConfigRecord => ({
  type: 'shell',
  name: 'cf-redis',
  command: 'echo',
  args: ['123'],
});
