import { createConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  SiteCertificateArn: string,
  Domain: string,
  Stage: string,
  HostedZoneId: string
};

export const Config = (inputs: Inputs) => createConfigRecord({
  type: 'cloudformation',
  name: 'cf-cdn',
  cfpath: './cf.yaml',
  inputs
});
