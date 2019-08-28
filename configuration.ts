import { Configuration, createConfigRecord } from '@inf/vars/configure';
import { vars } from '@inf/vars';

import * as AwsInfo from '@inf/cf-awsinfo/config';
import * as ServerlessPostgress from '@inf/cf-serverless-postgres/config';
import * as Redis from '@inf/cf-redis/config';
import * as Cognito from '@inf/cf-cognito/config';
import * as Cert from '@inf/cf-cert/config';
import * as Gen from '@inf/cf-gen/config';
import * as CDN from '@inf/cf-cdn/config';
import * as Sls from '@inf/cf-sls/config';

enum ServiceIds {
  RDS = 1,
};

const configuration: Configuration = {
  region: vars.AWS_REGION,
  stage: vars.STAGE,
  modules: [
    AwsInfo.Config({ rootEnv: '.env' }),
    ServerlessPostgress.Config({
      Stage: vars.STAGE,
      DatabaseName: 'main',
      MasterUsername: vars.MASTER_USERNAME,
      MasterUserPassword: vars.MASTER_USER_PASSWORD,
      MinCapacity: 2,
      MaxCapacity: 2
    }),
    Redis.Config({}),
    Cert.Config({ Domain: vars.DOMAIN }),
    Cognito.Config({
      Stage: vars.STAGE,
      Domain: vars.DOMAIN,
      InvitationEmailSubject: `Welcome To ${vars.NICE_NAME}`,
      VerificationEmailSubject: `${vars.NICE_NAME} requires your verification`,
      FromEmail: `verification@${vars.NICE_NAME}`
    }),
    outputs => Gen.Config({
      Stage: vars.STAGE,
      MasterUsername: vars.MASTER_USERNAME,
      MasterUserPassword: vars.MASTER_USER_PASSWORD,
      RDSServiceId: ServiceIds.RDS,
      RDSClusterEndpointAddress: outputs('CF_SERVERLESS_POSTGRES_RDSClusterEndpointAddress'),
      ledgerPath: 'ledger.scm'
    }),
    createConfigRecord({
      type: 'shell',
      name: 'cf-api',
      command: './api/make',
      args: [vars.STAGE == 'local' ? 'build' : 'deploy'],
      dependsOn: ['./api/src/**/*.ts', './api/serverless.yml'],
      outputs: {
        GraphQLEndpoint: vars.STAGE == 'local' ? 'http://0.0.0.0:3000/graphql' : { outputMatcher: /Service Information[\s\S.]+endpoints:[\s\S.]+POST - (.+)$/gm }
      }
    }),
    createConfigRecord({
      type: 'shell',
      name: 'cf-site',
      command: './site/make',
      args: [vars.STAGE == 'local' ? 'build' : 'deploy'],
      dependsOn: [
        './site/src/**/*.(ts|tsx|css)',
        './site/serverless.yml',
        './site/*.js',
        './site/static/**/*'
      ],
      outputs: {
        SiteURL: { outputMatcher: /Serverless: Success! Your site should be available at (.*)/ }
      }
    }),
    outputs => CDN.Config({
      SiteCertificateArn: outputs('CF_CERT_CertificateArn'),
      Domain: vars.DOMAIN,
      Stage: vars.STAGE,
      HostedZoneId: outputs('CF_AWSINFO_HostedZoneId'),
    }),
    outputs => Sls.Config({
      rootDir: './api/src',
      handlers: {
        api: { filename: 'api.ts', entrypoint: 'handler' },
        auth: { filename: 'auth.ts', entrypoint: 'handler' }
      },
      stage: vars.STAGE,
      region: vars.AWS_REGION,
      accountId: vars.AWS_ACCOUNT_ID,
      cognitoUserPoolId: outputs('CF_COGNITO_UserPoolId'),
      securityGroupIds: [outputs('CF_AWSINFO_SecurityGroup_default')],
      subnetIds: [outputs('CF_AWSINFO_Subnet1'), outputs('CF_AWSINFO_Subnet2')],
    })
  ]
}

export default configuration;