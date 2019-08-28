#!/usr/bin/env node

// @ts-check

const AWS = require('aws-sdk');
const path = require('path');
const fs = require('fs');
const { vars } = require('@inf/vars');

const writeJs = (outPath, data) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `// this file has been automatically generated\n\nexports.vars = {\n${out}};`);
};

const writeTs = (outPath, data) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `// this file has been automatically generated\n\nexport const vars = {\n${out}};`);
};

const main = async () => {
  if (vars.STAGE == 'local') {
    return {
      AWS_REGION: vars.AWS_REGION,
      VPC_ID: 'LOCAL_UNUSED',
      HostedZoneId: 'LOCAL_UNUSED',
      Subnet1: 'LOCAL_UNUSED',
      Subnet2: 'LOCAL_UNUSED',
      SecurityGroup_default: 'LOCAL_UNUSED',
    };
  } else {
    AWS.config = new AWS.Config({
      region: vars.AWS_REGION,
      accessKeyId: vars.AWS_ACCESS_KEY_ID,
      secretAccessKey: vars.AWS_SECRET_ACCESS_KEY,
    });
    const ec2 = new AWS.EC2({ apiVersion: '2016-11-15' });
    const route53 = new AWS.Route53({ apiVersion: '2013-04-01' });

    const ret = await ec2.describeVpcs({}).promise();
    console.assert(ret.Vpcs.length != 0);
    const VPC_ID = ret.Vpcs[0].VpcId;

    const data = await ec2.describeSubnets({
      Filters: [{ Name: 'vpc-id', Values: [VPC_ID] }]
    }).promise();

    const props = {};

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
  }
};

if (process.argv.length < 2) {
  console.log(`invalid usage: ${path.basename(process.argv[1])} (<outpath>)`);
  process.exit(1);
} else {
  const outpath = process.argv.length == 3 ? process.argv[2] : '';
  main().then(result => {
    process.stdout.write(JSON.stringify(result));
    const output = Object.entries(result).map(([k, v]) => `${k}=${v}`).join('\n');
    if (outpath)
      fs.writeFileSync(outpath, output);

    const tsdir = `${__dirname}/src`;
    if (!fs.existsSync(tsdir))
      fs.mkdirSync(tsdir);
    const outvars = { AWS_REGION: vars.AWS_ACCESS_KEY_ID }
    writeTs(`${tsdir}/vars.ts`, outvars);
    writeJs(`${tsdir}/vars.js`, outvars);
  }).catch(err => {
    if (outpath)
      fs.existsSync(outpath) && fs.unlinkSync(outpath);
    console.error(err);
    process.exit(1);
  });
}
