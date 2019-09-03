import * as AWS from 'aws-sdk';
import { vars } from './vars';

export const up = async () => {
  const ec2 = new AWS.EC2({
    apiVersion: '2016-11-15',
    region: vars.STAGE,
  });

  console.log('describe spot instance requests...');
  const ires = await ec2.describeSpotInstanceRequests().promise();
  if (!ires.SpotInstanceRequests) throw new Error('invalid spot instance request result');
  const reqs = ires.SpotInstanceRequests.filter(r => (r.State == 'open' || r.State == 'active') && r.LaunchSpecification && r.LaunchSpecification.KeyName == vars.PUB_KEY);
  const reqIds = reqs.map(r => r.SpotInstanceRequestId);
  if (reqIds.length > 0) {
    console.log('already started');
    return;
  }

  const vdesc = await ec2.describeVolumes({
    VolumeIds: [vars.VOLUME_ID]
  }).promise();
  if (!vdesc.Volumes || vdesc.Volumes.length != 1) throw new Error(`invalid volume count ${vdesc.Volumes && vdesc.Volumes.length || 0}`);

  console.log('request spot instance...');

  const result = await ec2.requestSpotInstances({
    LaunchSpecification: {
      ImageId: vars.PREBOOT_IMAGE_ID,
      InstanceType: vars.INSTANCE_TYPE,
      KeyName: vars.PUB_KEY,
      IamInstanceProfile: { Name: vars.SPOT_ML_INSTANCE_PROFILE_NAME },
      EbsOptimized: false,
      Placement: {
        AvailabilityZone: vars.AVAILABILITY_ZONE,
      },
      NetworkInterfaces: [{
        DeviceIndex: 0,
        SubnetId: vars.SUBNET,
        Groups: [vars.SECURITY_GROUP],
        AssociatePublicIpAddress: true
      }],
      UserData: Buffer.from(`#!/bin/sh

touch /root/log.txt
echo "booting..." > /root/log.txt 2>&1
set -e
export TERM="linux"
curl http://169.254.169.254/latest/meta-data/iam/security-credentials/${vars.AWS_REGION}-mlspot-SpotMLRole-${vars.STAGE} > /root/.creds
mkdir -p /root/.aws
echo "[default]" > /root/.aws/credentials
echo "aws_access_key_id=\`jq -r '.AccessKeyId' /root/.creds\`" >> /root/.aws/credentials
echo "aws_secret_access_key=${'`'}jq -r '.SecretAccessKey' /root/.creds\`" >> /root/.aws/credentials
echo "aws_session_token=\`jq -r '.Token' /root/.creds\`" >> /root/.aws/credentials
echo "[default]" > /root/.aws/config
echo "region=${vars.AWS_REGION}" >> /root/.aws/config
echo "output=json" >> /root/.aws/config
echo "attching volume..." >> /root/log.txt 2>&1
aws ec2 describe-volumes --volume-ids ${vars.VOLUME_ID} >> /root/log.txt 2>&1
aws ec2 attach-volume --dev /dev/xvdf --instance-id \`wget -q -O - http://169.254.169.254/latest/meta-data/instance-id\` --volume-id ${vars.VOLUME_ID} >> /root/log.txt 2>&1
aws ec2 wait volume-in-use --volume-ids ${vars.VOLUME_ID} >> /root/log.txt 2>&1
rm /root/.creds
NEEDS_FORMAT="\`sudo file -s /dev/xvdf\`"
if [ "$NEEDS_FORMAT" = "/dev/xvdf: data" ]; then
  echo "formating..." >> /root/log.txt 2>&1
  sudo mkfs -t ext4 /dev/xvdf >> /root/log.txt 2>&1
else
  echo "already formatted" >> /root/log.txt 2>&1
fi
sleep 1
echo "mounting..." >> /root/log.txt 2>&1
mkdir -p /workspace
sudo mount /dev/xvdf /workspace >> /root/log.txt 2>&1
chown -R ubuntu:ubuntu /workspace
cd /workspace/daemon/
make install
make start
echo "done" >> /root/log.txt 2>&1
`).toString('base64')
    },
    InstanceInterruptionBehavior: 'stop',
    Type: 'persistent',
    SpotPrice: vars.BID_PRICE,
  }).promise();
  if (!result.SpotInstanceRequests || result.SpotInstanceRequests.length != 1) throw new Error('invalid spot instance request count');
  const requestId = result.SpotInstanceRequests[0].SpotInstanceRequestId;
  if (!requestId) throw new Error('invalid request id');

  console.log('wait for spot instance request to fulfill...', requestId);
  await ec2.waitFor('spotInstanceRequestFulfilled', { SpotInstanceRequestIds: [requestId] }).promise();

  console.log('describe spot instance requests...');
  const res = await ec2.describeSpotInstanceRequests({
    SpotInstanceRequestIds: [requestId],
  }).promise();
  if (!res.SpotInstanceRequests || res.SpotInstanceRequests.length != 1) throw new Error('invalid describe spot instance requests');
  console.log(res.SpotInstanceRequests);
  const instanceId = res.SpotInstanceRequests[0].InstanceId;
  if (!instanceId) throw new Error('invalid instanceid');

  console.log('waiting for instance to run...', instanceId);
  await ec2.waitFor('instanceRunning', {
    InstanceIds: [instanceId]
  }).promise();
}

export const down = async () => {
  const ec2 = new AWS.EC2({
    apiVersion: '2016-11-15',
    region: vars.AWS_REGION
  });

  console.log('describe spot instance requests...');
  const res = await ec2.describeSpotInstanceRequests({}).promise();
  if (!res.SpotInstanceRequests) return;
  const reqs = res.SpotInstanceRequests.filter(r => r.State == 'active' && r.LaunchSpecification && r.LaunchSpecification.KeyName == vars.PUB_KEY);

  const SpotInstanceRequestIds = reqs.map(r => r.SpotInstanceRequestId).filter(s => s) as string[];
  if (SpotInstanceRequestIds.length > 0) {
    console.log('cancel spot instance requests...', SpotInstanceRequestIds.length);
    await ec2.cancelSpotInstanceRequests({ SpotInstanceRequestIds }).promise();
  }

  const InstanceIds = reqs.map(r => r.InstanceId).filter(s => s) as string[];
  if (InstanceIds.length > 0) {
    console.log('terminate instances...', InstanceIds.length);
    await ec2.terminateInstances({ InstanceIds }).promise();

    console.log('waiting for instance to terminate...');
    await ec2.waitFor('instanceTerminated', { InstanceIds }).promise();
  }
}