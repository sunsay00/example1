import { ConfigRecord } from '@inf/configure';

export type Inputs = {
  Stage: string,
  DatabaseName: string,
  MasterUsername: string,
  MasterUserPassword: string,
  MinCapacity: number,
  MaxCapacity: number
};

export const Config = (inputs: Inputs): ConfigRecord => ({
  type: 'cloudformation',
  name: 'cf-serverless-postgres',
  key: 'SLSPG',
  inputs
});
