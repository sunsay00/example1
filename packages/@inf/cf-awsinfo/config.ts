import * as AWS from 'aws-sdk';
import { vars } from '@inf/hookops/vars';
import { createModule, useGlobals, vartools, useDependsOn, useCache } from '@inf/hookops';

export const useAwsInfo = (props: {
  rootEnv: string
}) => createModule(async () => {

  const { stage } = useGlobals();

  const ret = {
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
    return ret;
  } else {
    let dirty = false;

    await useDependsOn(async () => {
      dirty = true;
    }, [props.rootEnv]);

    return await useCache(async () => {
      const cfg = {
        region: vars.AWS_REGION,
        accessKeyId: vartools.expand(vars.AWS_ACCESS_KEY_ID),
        secretAccessKey: vartools.expand(vars.AWS_SECRET_ACCESS_KEY),
      };
      const ec2 = new AWS.EC2({ apiVersion: '2016-11-15', ...cfg });
      const route53 = new AWS.Route53({ apiVersion: '2013-04-01', ...cfg });

      const result = await ec2.describeVpcs({}).promise();
      console.assert(result.Vpcs.length != 0);
      const VPC_ID = result.Vpcs[0].VpcId;

      const data = await ec2.describeSubnets({
        Filters: [{ Name: 'vpc-id', Values: [VPC_ID] }]
      }).promise();

      ret['AWS_REGION'] = vars.AWS_REGION;
      ret['VPC_ID'] = VPC_ID;

      const subnets = data.Subnets.sort((a, b) => a.AvailabilityZone.localeCompare(b.AvailabilityZone));
      subnets.forEach((s, i) => ret[`Subnet${i + 1}`] = s.SubnetId);
      subnets.forEach((s, i) => ret[`AvailabilityZone${i + 1}`] = s.AvailabilityZone);

      const zones = await route53.listHostedZonesByName().promise();
      const zone = zones.HostedZones.find(z => z.Name == `${vars.DOMAIN}.`);
      if (zone) {
        const split = zone.Id.split('/');
        if (split.length == 3) {
          ret['HostedZoneId'] = split[split.length - 1];
        }
      }
      if (!ret['HostedZoneId'])
        throw new Error(`failed to determine hostedzone for '${vars.DOMAIN}'`);

      const sgs = await ec2.describeSecurityGroups({}).promise();
      sgs.SecurityGroups.map(sg => ret[`SecurityGroup_${sg.GroupName}`] = sg.GroupId);

      return ret;
    }, dirty);
  }
});
