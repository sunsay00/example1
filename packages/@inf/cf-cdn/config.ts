import { ConfigRecord } from '@inf/vars/configure';

export type Inputs = {
  SiteCertificateArn: string,
  Domain: string,
  Stage: string,
  HostedZoneId: string
};

export const Config = (inputs: Inputs): ConfigRecord => ({
  type: 'cloudformation',
  name: 'cf-cdn',
  inputs
});
