import { Configuration } from '@inf/configure';
import { vars } from '@inf/vars';

import * as AwsInfo from '@inf/awsinfo/config';
import * as ServerlessPostgress from '@inf/cf-serverless-postgres/config';
import * as Cognito from '@inf/cf-cognito/config';
import * as Cert from '@inf/cf-cert/config';
import * as Gen from '@inf/gen/config';

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
    /*
    {
      type: 'shell',
      key: 'API',
      cwd: './api',
      command: 'make',
      args: ['configure'],
      outfile: '.envs.lambda',
    },
    Gen.Config(),
    */
  ]
}

export default configuration;
