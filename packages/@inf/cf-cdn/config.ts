import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  SiteCertificateArn: string,
  Domain: string,
  HostedZoneId: string
}) => createConfigRecord(async ({ stage }) => ({
  type: 'cloudformation',
  rootDir: __dirname,
  cfpath: './cf.yaml',
  inputs: {
    ...inputs,
    Stage: stage
  },
  outputs: {}
}));
