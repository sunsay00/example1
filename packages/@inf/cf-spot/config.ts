import { createConfigRecord } from '@inf/vars/configure';

export const Config = (inputs: {
  vpcId: string,
  availabilityZone: string,
  bidPrice: number,
  prebootImageId: string,
  instanceType: string,
  pubKey: string,
  subnet: string,
}) => createConfigRecord(async ({ stage, region }, reg) => {

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
      PUB_KEY: inputs.pubKey,
      SUBNET: inputs.subnet,
      AVAILABILITY_ZONE: inputs.availabilityZone,
      SECURITY_GROUP: spot.SecurityGroupId,
      VOLUME_ID: spot.VolumeId,
      SPOT_ML_INSTANCE_PROFILE_NAME: spot.InstanceProfileName
    },
    outputs: {}
  };
});