import { createConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  Stage: string,
  SecurityGroupIds: string[],
  SubnetIds: string[],
}

export const Config = (inputs: Inputs) => createConfigRecord({
  type: 'cloudformation',
  name: 'cf-simplelambda',
  cfpath: './cf.yaml',
  inputs
});