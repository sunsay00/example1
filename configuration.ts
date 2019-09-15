import { Configuration } from '@inf/hookops';
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

import { useLamTest } from './cf-lam-test/config';
import { useApi } from './api/config';
import { useSite } from './site/config';
import { useMobile } from './mobile/config';

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

    const cognito = await useCognito({
      Domain: vars.DOMAIN,
      InvitationEmailSubject: `Welcome To ${vars.NICE_NAME}`,
      VerificationEmailSubject: `${vars.NICE_NAME} requires your verification`,
      FromEmail: `verification@${vars.NICE_NAME}`
    });

    // local uses dev cognito credentials
    const cog = vars.STAGE != 'local' ? cognito : {
      CognitoIdentityPoolId: 'us-east-1:744bb2da-6b84-4150-ba40-5f6c00d79e87',
      UserPoolId: 'us-east-1_vaHQ7ND3L',
      MobileUserPoolClientId: '5aeqscghjc8lguth4f6qosc8d7',
      WebUserPoolClientId: '1djqk831709slrd9olr4q0qh7d'
    };

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

    await useCDN({
      SiteCertificateArn: cert.CertificateArn,
      Domain: vars.DOMAIN,
      HostedZoneId: aws.HostedZoneId,
    });

    const endpoints = await useApi({
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

    await useSite({
      graphqlEndpoint: endpoints.api,
      region: vars.AWS_REGION,
      cognito: {
        identityPoolId: cog.CognitoIdentityPoolId,
        userPoolId: cog.UserPoolId,
        clientId: cog.WebUserPoolClientId,
      }
    });

    await useLamTest({
      dbUrl: gen.dbUrl,
      securityGroupIds: [aws.SecurityGroup_default],
      subnetIds: [aws.Subnet1, aws.Subnet2],
    });

    await useMobile({
      graphqlEndpoint: endpoints.api,
      region: vars.AWS_REGION,
      cognito: {
        identityPoolId: cog.CognitoIdentityPoolId,
        userPoolId: cog.UserPoolId,
        clientId: cog.MobileUserPoolClientId,
      }
    });
  }
};

export default configuration;
