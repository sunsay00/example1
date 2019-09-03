import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  rootEnv: string
}) => createConfigRecord({
  type: 'shell',
  rootDir: __dirname,
  command: `${__dirname}/make`,
  dependsOn: [inputs.rootEnv],
  args: ['configure'],
  outputs: {
    AWS_REGION: '',
    VPC_ID: '',
    HostedZoneId: '',
    Subnet1: '',
    Subnet2: '',
    SecurityGroup_default: '',
    AvailabilityZone1: '',
    AvailabilityZone2: ''
  }
});
