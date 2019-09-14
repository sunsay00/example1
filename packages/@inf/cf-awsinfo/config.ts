import * as AWS from 'aws-sdk';
import { vars } from '@inf/hookops/vars';
import { createModule, useGlobals, vartools, useDependsOn, useCache } from '@inf/hookops';

export const useAwsInfo = (inputs: {
  rootEnv: string
}) => createModule('cf-awsinfo', async () => {

  const { stage } = useGlobals();

  const props = {
    AWS_REGION: vars.AWS_REGION,
    VPC_ID: 'LOCAL_UNDEFINED',
    HostedZoneId: 'LOCAL_UNDEFINED',
    Subnet1: 'LOCAL_UNDEFINED',
    Subnet2: 'LOCAL_UNDEFINED',
    SecurityGroup_default: 'LOCAL_UNDEFINED',
    AvailabilityZone1: 'LOCAL_UNDEFINED',
    AvailabilityZone2: 'LOCAL_UNDEFINED',
  };

  if (stage == 'local') {
    return props;
  } else {
    let dirty = false;

    await useDependsOn(async () => {
      dirty = true;
    }, [inputs.rootEnv]);

    return await useCache(async () => {
      const cfg = {
        region: vars.AWS_REGION,
        accessKeyId: vartools.expand(vars.AWS_ACCESS_KEY_ID),
        secretAccessKey: vartools.expand(vars.AWS_SECRET_ACCESS_KEY),
      };
      const ec2 = new AWS.EC2({ apiVersion: '2016-11-15', ...cfg });
      const route53 = new AWS.Route53({ apiVersion: '2013-04-01', ...cfg });

      const ret = await ec2.describeVpcs({}).promise();
      console.assert(ret.Vpcs.length != 0);
      const VPC_ID = ret.Vpcs[0].VpcId;

      const data = await ec2.describeSubnets({
        Filters: [{ Name: 'vpc-id', Values: [VPC_ID] }]
      }).promise();

      props['AWS_REGION'] = vars.AWS_REGION;
      props['VPC_ID'] = VPC_ID;

      const subnets = data.Subnets.sort((a, b) => a.AvailabilityZone.localeCompare(b.AvailabilityZone));
      subnets.forEach((s, i) => props[`Subnet${i + 1}`] = s.SubnetId);
      subnets.forEach((s, i) => props[`AvailabilityZone${i + 1}`] = s.AvailabilityZone);

      const zones = await route53.listHostedZonesByName().promise();
      const zone = zones.HostedZones.find(z => z.Name == `${vars.DOMAIN}.`);
      if (zone) {
        const split = zone.Id.split('/');
        if (split.length == 3) {
          props['HostedZoneId'] = split[split.length - 1];
        }
      }
      if (!props['HostedZoneId'])
        throw new Error(`failed to determine hostedzone for '${vars.DOMAIN}'`);

      const sgs = await ec2.describeSecurityGroups({}).promise();
      sgs.SecurityGroups.map(sg => props[`SecurityGroup_${sg.GroupName}`] = sg.GroupId);

      return props;
    }, dirty);
  }
});
