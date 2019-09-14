import * as fs from 'fs';
import * as AWS from 'aws-sdk';

import { createModule, useEffect, useMemo, useGlobals, useShell } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';
import { useVarsWriter, useCloudFormation } from '@inf/hooks';

export const useSpot = (inputs: {
  id: string,
  vpcId: string,
  availabilityZone: string,
  bidPrice: number,
  prebootImageId: string,
  instanceType: string,
  pubKey: { name: string, path: string },
  subnet: string,
  shellUser: string,
  volumeSizeInGB: number,
}) => createModule('cf-spot', async () => {
  const { stage } = useGlobals();

  const ec2 = new AWS.EC2({
    apiVersion: '2016-11-15',
    region: vars.AWS_REGION
  });

  if (stage == 'local') {

    return { host: '' };

  } else {
    if (!fs.existsSync(inputs.pubKey.path))
      throw new Error('public key not found');

    await useEffect(async () => {
      const keys = await ec2.describeKeyPairs({
        Filters: [{ Name: 'key-name', Values: [inputs.pubKey.name] }]
      }).promise();

      if (keys.KeyPairs && keys.KeyPairs.length == 0) {
        await ec2.importKeyPair({
          KeyName: inputs.pubKey.name,
          PublicKeyMaterial: fs.readFileSync(inputs.pubKey.path),
        }).promise();
      }
    }, [inputs.pubKey.name, inputs.pubKey.path]);

    const spotid = `cf-spot--${inputs.id}`;

    const spot = await useCloudFormation({
      id: spotid,
      cfyamlpath: `${__dirname}/cf.yaml`,
      inputs: {
        VpcId: inputs.vpcId,
        AvailabilityZone: inputs.availabilityZone,
        Stage: stage,
        VolumeSize: inputs.volumeSizeInGB,
        VolumeTagName: inputs.pubKey.name,
      },
      defaultOutputs: {
        SecurityGroupId: '',
        VolumeId: '',
        InstanceProfileName: ''
      }
    });

    await useEffect(async () => {
      useVarsWriter('shell', __dirname, {
        ID: spotid,
        STAGE: stage,
        AWS_REGION: vars.AWS_REGION,
        BID_PRICE: `${inputs.bidPrice}`,
        PREBOOT_IMAGE_ID: inputs.prebootImageId,
        INSTANCE_TYPE: inputs.instanceType,
        PUB_KEY: inputs.pubKey.name,
        SUBNET: inputs.subnet,
        AVAILABILITY_ZONE: inputs.availabilityZone,
        SECURITY_GROUP: spot.SecurityGroupId,
        VOLUME_ID: spot.VolumeId,
        SPOT_ML_INSTANCE_PROFILE_NAME: spot.InstanceProfileName,
        SHELL_USER: inputs.shellUser
      });
    }, [stage, vars.AWS_REGION, inputs, spot, spotid]);

    const publicdnsname = await useMemo(async () => {
      const { instanceId } = await useShell({
        command: 'make',
        args: ['up'],
        cwd: __dirname,
        outputMatchers: {
          instanceId: /InstanceId: '([^']+)'/gm
        }
      });

      const ret = await ec2.describeInstances({
        InstanceIds: [instanceId]
      }).promise();

      const publicdnsname = ret.Reservations && ret.Reservations.reduce<string | undefined>((a, r) =>
        a || r.Instances && r.Instances.reduce<string | undefined>((a, i) =>
          a || i.PublicDnsName, undefined), undefined);
      if (!publicdnsname)
        throw new Error('invalid to retrieve public dns name');

      return publicdnsname;
    }, []);

    return { host: `${inputs.shellUser}@${publicdnsname}` }
  }
});