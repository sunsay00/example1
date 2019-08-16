#!/usr/bin/env node

const AWS = require('aws-sdk');
const path = require('path');
const fs = require('fs');
const { verifyVars } = require('@inf/common');

const config = verifyVars({
  AWS_REGION: process.env.AWS_REGION,
  DOMAIN: process.env.DOMAIN,
});

const main = async () => {
  const ec2 = new AWS.EC2({ region: config.AWS_REGION });
  const route53 = new AWS.Route53({ apiVersion: '2013-04-01' });

  const ret = await ec2.describeVpcs({}).promise();
  console.assert(ret.Vpcs.length != 0);
  const VPC_ID = ret.Vpcs[0].VpcId;

  const data = await ec2.describeSubnets({
    Filters: [{ Name: 'vpc-id', Values: [VPC_ID] }]
  }).promise();

  const props = {
    VPC_ID,
  };

  const subnets = data.Subnets.sort((a, b) => a.AvailabilityZone.localeCompare(b.AvailabilityZone));
  subnets.forEach((s, i) => props[`Subnet${i + 1}`] = s.SubnetId);
  subnets.forEach((s, i) => props[`AvailabilityZone${i + 1}`] = s.AvailabilityZone);

  const zones = await route53.listHostedZonesByName().promise();
  const zone = zones.HostedZones.find(z => z.Name == `${config.DOMAIN}.`);
  if (zone) {
    const split = zone.Id.split('/');
    if (split.length == 3) {
      props['HostedZoneId'] = split[split.length - 1];
    }
  }
  if (!props['HostedZoneId'])
    throw new Error(`failed to determine hostedzone for '${config.DOMAIN}'`);

  return Object.entries(props).map(([k, v]) => `${k}=${v}`).join('\n');
};

if (process.argv.length != 3) {
  console.log(`invalid usage: ${path.basename(process.argv[1])} <outpath>`);
  process.exit(1);
} else {
  const outpath = process.argv[2];
  main().then(output => {
    fs.writeFileSync(outpath, output);
  }).catch(err => {
    fs.existsSync(outpath) && fs.unlinkSync(outpath);
    console.error(err);
    process.exit(1);
  });
}
