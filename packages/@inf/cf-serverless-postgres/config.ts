import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  DatabaseName: string,
  MasterUsername: string,
  MasterUserPassword: string,
  MinCapacity: number,
  MaxCapacity: number
}) => createConfigRecord(async ({ stage }) => ({
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
}));
