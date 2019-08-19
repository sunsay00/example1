import { Configuration } from '@inf/vars/configure';
import { vars } from '@inf/vars';

import * as AwsInfo from '@inf/cf-awsinfo/config';
import * as ServerlessPostgress from '@inf/cf-serverless-postgres/config';
import * as Cognito from '@inf/cf-cognito/config';
import * as Cert from '@inf/cf-cert/config';
import * as Gen from '@inf/cf-gen/config';

const configuration: Configuration = {
  region: vars.AWS_REGION,
  stage: vars.STAGE,
  modules: [
    AwsInfo.Config({
      dependsOn: ['.env'],
    }),
    ServerlessPostgress.Config({
      Stage: vars.STAGE,
      DatabaseName: 'main',
      MasterUsername: vars.MASTER_USERNAME,
      MasterUserPassword: vars.MASTER_USER_PASSWORD,
      MinCapacity: 2,
      MaxCapacity: 2
    }),
    Cert.Config({ Domain: vars.DOMAIN }),
    Cognito.Config({
      Stage: vars.STAGE,
      Domain: vars.DOMAIN,
      InvitationEmailSubject: `Welcome To ${vars.NICE_NAME}`,
      VerificationEmailSubject: `${vars.NICE_NAME} requires your verification`,
      FromEmail: `verification@${vars.NICE_NAME}`
    }),
    Gen.Config({
      ledgerPath: 'ledger.scm'
    }),
    {
      type: 'shell',
      name: 'cf-api',
      command: './api/make',
      args: ['configure'],
      dependsOn: ['./api/src/**/*.ts'],
      outputMatchers: {
        GraphQLEndpoint: /Service Information[\s\S.]+endpoints:[\s\S.]+POST - (.+)$/gm,
      }
    },
    {
      type: 'shell',
      name: 'cf-site-build',
      cwd: './site',
      command: 'gatsby',
      args: ['build', '--prefix-path'],
      dependsOn: ['./site/src/**/*', './site/serverless.yml', './site/*.js']
    },
    {
      type: 'shell',
      name: 'cf-site',
      cwd: './site',
      command: 'yarn',
      args: ['vars', 'serverless', 'client', 'deploy', '--stage', '{{STAGE}}', '--region', '{{AWS_REGION}}', '--no-confirm'],
      dependsOn: ['./site/public/**/*'],
      outputMatchers: {
        SiteURL: /Serverless: Success! Your site should be available at (.*)/
      }
    }
  ]
}

export default configuration;
