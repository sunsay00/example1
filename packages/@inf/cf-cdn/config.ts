import { createConfig } from '@inf/vars/configure';

export const Config = (inputs: {
  SiteCertificateArn: string,
  Domain: string,
  HostedZoneId: string
}) => createConfig(({ stage }) => ({
  clean: async () => ({}),
  up: async () => ({
    type: 'cloudformation',
    rootDir: __dirname,
    cfpath: './cf.yaml',
    inputs: {
      ...inputs,
      Stage: stage
    },
    outputs: {}
  })
}));
