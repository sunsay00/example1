import { createConfigRecord } from '@inf/vars/configure';

export type Inputs = {};

export const Config = (inputs: Inputs) => createConfigRecord({
  type: 'shell',
  name: 'cf-redis',
  command: 'echo',
  args: ['-n', 'NYI'],
});
