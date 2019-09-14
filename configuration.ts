import { Configuration, useGlobals } from '@inf/hookops';
import { useTunnel } from '@inf/hooks';
import { vars } from '@inf/hookops/vars';

import { useAwsInfo } from '@inf/cf-awsinfo/config';
import { useCert } from '@inf/cf-cert/config';
import { useSpot } from '@inf/cf-spot/config';
import { usePostgres } from '@inf/cf-serverless-postgres/config';
import { useRedis } from '@inf/cf-redis/config';
import { useCognito } from '@inf/cf-cognito/config';
import { useGen } from '@inf/cf-gen/config';
import { useCDN } from '@inf/cf-cdn/config';

import { useSite } from './site/config';
import { useLamTest } from './cf-lam-test/config';
import { useApi } from './api/config';

const ROOT_ENV = './.env';
const LEDGER_PATH = './ledger.scm';
const JUMP_PUB_KEY_PATH = './jump.pub';

enum ServiceIds {
  RDS = 1,
};

const configuration: Configuration = {
  stage: vars.STAGE,
  configure: async () => {

    const aws = await useAwsInfo({ rootEnv: ROOT_ENV });

    const cert = await useCert({ Domain: vars.DOMAIN });

    const spot = await useSpot({
      id: '1',
      vpcId: aws.VPC_ID,
      availabilityZone: aws.AvailabilityZone1,
      bidPrice: 0.007,
      prebootImageId: 'ami-009d6802948d06e52', // Amazon Linux 2 AMI (HVM), SSD Volume Type
      instanceType: 't3a.small',
      pubKey: { name: 'jump', path: JUMP_PUB_KEY_PATH },
      subnet: aws.Subnet1,
      shellUser: 'ec2-user',
      volumeSizeInGB: 8
    });

    const proxy = { host: spot.host, port: 54321 };

    await useRedis();

    const postgres = await usePostgres({
      DatabaseName: 'main',
      MasterUsername: vars.MASTER_USERNAME,
      MasterUserPassword: vars.MASTER_USER_PASSWORD,
      MinCapacity: 2,
      MaxCapacity: 2,
    });

    const tunnel = useTunnel({ target: postgres, proxy });

    const gen = await useGen({
      username: vars.MASTER_USERNAME,
      password: vars.MASTER_USER_PASSWORD,
      db: postgres,
      proxy,
      RDSServiceId: ServiceIds.RDS,
      ledgerPath: LEDGER_PATH,
      tunnel
    });

    const cog = await useCognito({
      Domain: vars.DOMAIN,
      InvitationEmailSubject: `Welcome To ${vars.NICE_NAME}`,
      VerificationEmailSubject: `${vars.NICE_NAME} requires your verification`,
      FromEmail: `verification@${vars.NICE_NAME}`
    });

    await useSite();

    await useCDN({
      SiteCertificateArn: cert.CertificateArn,
      Domain: vars.DOMAIN,
      HostedZoneId: aws.HostedZoneId,
    });

    await useApi({
      accountId: vars.AWS_ACCOUNT_ID,
      cognitoUserPoolId: cog.UserPoolId,
      securityGroupIds: [aws.SecurityGroup_default],
      subnetIds: [aws.Subnet1, aws.Subnet2],
      MasterUsername: vars.MASTER_USERNAME,
      MasterUserPassword: vars.MASTER_USER_PASSWORD,
      dbUrl: gen.dbUrl,
      dbTestUrl: gen.dbTestUrl,
      tunnel
    });

    await useLamTest({
      dbUrl: gen.dbUrl,
      securityGroupIds: [aws.SecurityGroup_default],
      subnetIds: [aws.Subnet1, aws.Subnet2],
    });
  }
};

export default configuration;