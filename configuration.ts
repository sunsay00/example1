import { Configuration, createConfigRecord } from '@inf/vars/configure';
import { vars } from '@inf/vars';

import * as AwsInfo from '@inf/cf-awsinfo/config';
import * as ServerlessPostgress from '@inf/cf-serverless-postgres/config';
import * as Redis from '@inf/cf-redis/config';
import * as Cognito from '@inf/cf-cognito/config';
import * as Cert from '@inf/cf-cert/config';
import * as Gen from '@inf/cf-gen/config';
import * as CDN from '@inf/cf-cdn/config';
import * as SlsApi from '@inf/cf-sls-api/config';
import * as Sls from '@inf/cf-sls/config';
import * as Spot from '@inf/cf-spot/config';

enum ServiceIds {
  RDS = 1,
};

const configuration: Configuration = {
  region: vars.AWS_REGION,
  stage: vars.STAGE,
  configure: async reg => {

    const aws = await reg(AwsInfo.Config({ rootEnv: '.env' }));

    const rds = await reg(ServerlessPostgress.Config({
      DatabaseName: 'main',
      MasterUsername: '{{MASTER_USERNAME}}',
      MasterUserPassword: '{{MASTER_USER_PASSWORD}}',
      MinCapacity: 2,
      MaxCapacity: 2,
    }));

    await reg(Redis.Config({}));

    const cert = await reg(Cert.Config({ Domain: vars.DOMAIN }));

    const cog = await reg(Cognito.Config({
      Domain: vars.DOMAIN,
      InvitationEmailSubject: `Welcome To ${vars.NICE_NAME}`,
      VerificationEmailSubject: `${vars.NICE_NAME} requires your verification`,
      FromEmail: `verification@${vars.NICE_NAME}`
    }));

    await reg(Spot.Config({
      vpcId: aws.VPC_ID,
      availabilityZone: aws.AvailabilityZone1,
      bidPrice: 0.007,
      prebootImageId: 'ami-009d6802948d06e52', // Amazon Linux 2 AMI (HVM), SSD Volume Type
      instanceType: 't3a.small',
      pubKey: { name: 'jump', path: './jump.pub' },
      subnet: aws.Subnet1,
      shellUser: 'ec2-user',
      volumeSizeInGB: 10
    }));

    const gen = await reg(Gen.Config({
      MasterUsername: '{{MASTER_USERNAME}}',
      MasterUserPassword: '{{MASTER_USER_PASSWORD}}',
      RDSServiceId: ServiceIds.RDS,
      RDSClusterEndpointAddress: rds.RDSClusterEndpointAddress,
      ledgerPath: 'ledger.scm',
      rdsProxy: vars.STAGE == 'local' ? undefined : {
        proxyHost: 'ec2-user@ec2-52-73-213-184.compute-1.amazonaws.com',
        localPort: 54321
      }
    }));

    await reg(createConfigRecord({
      type: 'shell',
      rootDir: './site',
      command: './site/make',
      args: [vars.STAGE == 'local' ? 'build' : 'deploy'],
      dependsOn: [
        './site/src/**/*.(ts|tsx|css)',
        './site/serverless.yml',
        './site/*.js',
        './site/static/**/*'
      ],
      outputMatchers: {
        SiteURL: /Serverless: Success! Your site should be available at (.*)/
      },
      outputs: {
        SiteURL: ''
      }
    }));

    await reg(CDN.Config({
      SiteCertificateArn: cert.CertificateArn,
      Domain: vars.DOMAIN,
      HostedZoneId: aws.HostedZoneId,
    }));

    await reg(SlsApi.Config({
      id: 'cf-sls-api',
      dependsOn: ['./api/package.json', './api/src/**/*.ts'],
      accountId: vars.AWS_ACCOUNT_ID,
      cognitoUserPoolId: cog.UserPoolId,
      securityGroupIds: [aws.SecurityGroup_default],
      subnetIds: [aws.Subnet1, aws.Subnet2],
      localMasterUsername: 'root',
      localMasterUserPassword: 'for-development-only',
      apiHandler: {
        DB_URL: gen.DB_URL,
        DB_TEST_URL: gen.DB_TEST_URL,
        packageJsonPath: './api/package.json',
        filepath: 'src/api.ts',
        entrypoint: 'handler',
      },
    }));

    await reg(Sls.Config({
      id: 'cf-sls-test',
      rootDir: './cf-sls-test',
      dependsOn: ['./cf-sls-test/package.json', './cf-sls-test/**/*.ts'],
      handlers: {
        test: {
          vars: {
            DB_URL: gen.DB_URL
          },
          packageJsonPath: './cf-sls-test/package.json', filepath: 'index.ts', entrypoint: 'handler'
        }
      },
      webpackIgnore: /^pg-native$/,
      slsVpc: {
        securityGroupIds: [aws.SecurityGroup_default],
        subnetIds: [aws.Subnet1, aws.Subnet2],
      }
    }));

  }
};

export default configuration;