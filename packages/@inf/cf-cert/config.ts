import { createConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  Domain: string,
};

export type Outputs = {
  CertificateArn: string
}

export const Config = (inputs: Inputs) => createConfigRecord({
  type: 'cloudformation',
  name: 'cf-cert',
  cfpath: './cf.yaml',
  inputs,
  outputs: ['CertificateArn'] as (keyof Outputs)[]
});
