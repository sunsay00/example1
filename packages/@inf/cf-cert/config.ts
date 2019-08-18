import { ConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  Domain: string,
};

export type Outputs = {
  CertificateArn: string
}

export const Config = (inputs: Inputs): ConfigRecord => ({
  type: 'cloudformation',
  name: 'cf-cert',
  inputs,
  outputs: ['CertificateArn'] as (keyof Outputs)[]
});
