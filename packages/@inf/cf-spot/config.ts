import { createConfigRecord } from '@inf/vars/configure';
import * as fs from 'fs';

import * as AWS from 'aws-sdk';

export const Config = (inputs: {
  vpcId: string,
  availabilityZone: string,
  bidPrice: number,
  prebootImageId: string,
  instanceType: string,
  pubKey: { name: string, path: string },
  subnet: string,
  shellUser: string,
}) => createConfigRecord(async ({ stage, region }, reg) => {

  const ec2 = new AWS.EC2({
    apiVersion: '2016-11-15',
    region: region
  });

  if (!fs.existsSync(inputs.pubKey.path))
    throw new Error('public key not found');

  const keys = await ec2.describeKeyPairs({
    Filters: [{ Name: 'key-name', Values: [inputs.pubKey.name] }]
  }).promise();

  if (keys.KeyPairs.length == 0) {
    await ec2.importKeyPair({
      KeyName: inputs.pubKey.name,
      PublicKeyMaterial: fs.readFileSync(inputs.pubKey.path),
    }).promise();
  }

  const spot = await reg(createConfigRecord({
    type: 'cloudformation',
    rootDir: __dirname,
    cfpath: './cf.yaml',
    inputs: {
      VpcId: inputs.vpcId,
      AvailabilityZone: inputs.availabilityZone,
      Stage: stage
    },
    outputs: {
      SecurityGroupId: '',
      VolumeId: '',
      InstanceProfileName: ''
    }
  }));

  return {
    type: 'replace-vars',
    rootDir: __dirname,
    fqModuleId: 'cf-spot',
    id: 'replace-vars',
    silent: true,
    vars: {
      STAGE: stage,
      AWS_REGION: region,
      BID_PRICE: `${inputs.bidPrice}`,
      PREBOOT_IMAGE_ID: inputs.prebootImageId,
      INSTANCE_TYPE: inputs.instanceType,
      PUB_KEY: inputs.pubKey.name,
      SUBNET: inputs.subnet,
      AVAILABILITY_ZONE: inputs.availabilityZone,
      SECURITY_GROUP: spot.SecurityGroupId,
      VOLUME_ID: spot.VolumeId,
      SPOT_ML_INSTANCE_PROFILE_NAME: spot.InstanceProfileName,
      SHELL_USER: inputs.shellUser,
    },
    outputs: {}
  };
});