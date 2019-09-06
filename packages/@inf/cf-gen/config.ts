import { createConfig } from '@inf/vars/configure';

export const Config = (inputs: {
  MasterUsername: string,
  MasterUserPassword: string,
  RDSServiceId: number,
  RDSClusterEndpointAddress: string,
  ledgerPath: string,
  rdsProxy?: {
    proxyHost: string,
    localPort: number
  }
}) => createConfig(({ stage }) => ({
  clean: async () => ({
    DB_URL: '', DB_TEST_URL: ''
  }),
  up: async () => {
    const port = inputs.rdsProxy ? inputs.rdsProxy.localPort : 5432;
    const dbhost = inputs.rdsProxy ? 'localhost' : inputs.RDSClusterEndpointAddress;
    const DB_URL = `postgres://${inputs.MasterUsername}:${inputs.MasterUserPassword}@${dbhost}:${port}/main${stage}`;
    const DB_TEST_URL = `postgres://${inputs.MasterUsername}:${inputs.MasterUserPassword}@${dbhost}:${port}/test${stage}`;
    return {
      type: 'shell',
      rootDir: __dirname,
      command: 'make',
      env: inputs.rdsProxy ? {
        SERVICE_ID: `${inputs.RDSServiceId}`,
        DB_URL,
        RDS_HOST: inputs.RDSClusterEndpointAddress,
        PROXY_HOST: inputs.rdsProxy.proxyHost,
        LOCAL_PORT: `${inputs.rdsProxy.localPort}`
      } : {
          SERVICE_ID: `${inputs.RDSServiceId}`,
          DB_URL,
          RDS_HOST: '',
          PROXY_HOST: '',
          LOCAL_PORT: ''
        },
      args: ['-f', `${__dirname}/Makefile`, 'configure'],
      dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
      vars: inputs.rdsProxy ? {
        DB_URL, DB_TEST_URL,
        RDS_HOST: inputs.RDSClusterEndpointAddress,
        PROXY_HOST: inputs.rdsProxy.proxyHost,
        LOCAL_PORT: `${inputs.rdsProxy.localPort}`
      } : {
          DB_URL, DB_TEST_URL,
          RDS_HOST: '',
          PROXY_HOST: '',
          LOCAL_PORT: '0'
        },
      outputs: { DB_URL, DB_TEST_URL }
    };
  }
}));
