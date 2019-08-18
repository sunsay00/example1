import { ConfigRecord } from '@inf/vars/configure';

export const Config = (opts: { ledgerPath: string }): ConfigRecord => ({
  type: 'shell',
  name: 'cf-gen',
  command: `${__dirname}/gen`,
  args: [],
  dependsOn: [`${__dirname}/generator/**/*.scm`, opts.ledgerPath]
});
