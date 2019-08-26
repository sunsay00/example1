import { ConfigRecord } from '@inf/vars/configure';

type Inputs = {
  Stage: string,
  MasterUsername: string,
  MasterUserPassword: string,
  RDSServiceId: number,
  RDSClusterEndpointAddress: string,
  ledgerPath: string,
}

export const Config = (opts: Inputs): ConfigRecord => ({
  type: 'shell',
  name: 'cf-gen',
  command: 'make',
  env: {
    SERVICEID: `${opts.RDSServiceId}`,
    DBCONNSTR: `postgres://${opts.MasterUsername}:${opts.MasterUserPassword}@${opts.RDSClusterEndpointAddress}:5432/main${opts.Stage}`
  },
  args: ['-f', `${__dirname}/Makefile`, 'configure'],
  dependsOn: [`${__dirname}/generator/**/*.scm`, opts.ledgerPath]
});
