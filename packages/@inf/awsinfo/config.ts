import { ConfigRecord } from '@inf/configure';
import { resolve } from 'path';

export const Config = (inputs: { dependsOn?: string[] }): ConfigRecord => ({
  type: 'shell',
  key: 'AWS',
  cwd: __dirname,
  command: 'make',
  env: {
    INFILES: '',//inputs.infiles.map(i => resolve(i)).join(' '),
    OUTFILE: '',//resolve(inputs.outfile)
  },
  dependsOn: inputs.dependsOn,
  args: ['configure']
});
