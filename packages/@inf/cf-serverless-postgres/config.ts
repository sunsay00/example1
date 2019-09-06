import { createConfig } from '@inf/vars/configure';

export const Config = (inputs: {
  DatabaseName: string,
  MasterUsername: string,
  MasterUserPassword: string,
  MinCapacity: number,
  MaxCapacity: number
}) => createConfig(({ stage }) => ({
  clean: async () => ({
    RDSClusterEndpointAddress: ''
  }),
  up: async () => ({
    type: 'cloudformation',
    rootDir: __dirname,
    cfpath: './cf.yaml',
    inputs: {
      ...inputs,
      Stage: stage
    },
    outputs: {
      RDSClusterEndpointAddress: stage == 'local' ? 'localhost' : '',
    }
  })
}));
