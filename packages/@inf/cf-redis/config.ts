import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {

}) => createConfigRecord({
  type: 'shell',
  rootDir: __dirname,
  command: 'echo',
  args: ['-n', 'NYI'],
  outputs: {}
});
