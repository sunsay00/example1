import { createConfigRecord } from '@inf/vars/configure';

export const Config = (opts: {
  MasterUsername: string,
  MasterUserPassword: string,
  RDSServiceId: number,
  RDSClusterEndpointAddress: string,
  ledgerPath: string,
}) => createConfigRecord(async ({ stage }) => {
  const DB_URL = `postgres://${opts.MasterUsername}:${opts.MasterUserPassword}@${opts.RDSClusterEndpointAddress}:5432/main${stage}`;
  const DB_TEST_URL = `postgres://${opts.MasterUsername}:${opts.MasterUserPassword}@${opts.RDSClusterEndpointAddress}:5432/test${stage}`;
  return {
    type: 'shell',
    rootDir: __dirname,
    command: 'make',
    env: {
      SERVICE_ID: `${opts.RDSServiceId}`,
      DB_URL,
    },
    args: ['-f', `${__dirname}/Makefile`, 'configure'],
    dependsOn: [`${__dirname}/generator/**/*.scm`, opts.ledgerPath],
    vars: { DB_URL, DB_TEST_URL },
    outputs: { DB_URL, DB_TEST_URL }
  };
});
