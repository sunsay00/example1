import { verifyVars } from '@inf/common';
import { Configuration } from '@inf/configure';
import * as Cognito from '@inf/cf-cognito/config';
import * as ServerlessPostgress from '@inf/cf-serverless-postgres/config';
import * as Gen from '@inf/gen/config';
import * as Cert from '@inf/cf-cert/config';

const config = verifyVars({
  AWS_REGION: process.env.AWS_REGION,
  STAGE: process.env.STAGE,
  MASTER_USERNAME: process.env.MASTER_USERNAME,
  MASTER_USER_PASSWORD: process.env.MASTER_USER_PASSWORD,
  DOMAIN: process.env.DOMAIN,
  NICE_NAME: process.env.NICE_NAME
});

const configuration: Configuration = {
  region: config.AWS_REGION,
  stage: config.STAGE,
  modules: [
    ServerlessPostgress.Config({
      Stage: config.STAGE,
      DatabaseName: 'main',
      MasterUsername: config.MASTER_USERNAME,
      MasterUserPassword: config.MASTER_USER_PASSWORD,
      MinCapacity: 2,
      MaxCapacity: 2
    }),
    Cert.Config({
        Domain: config.DOMAIN
    }),
    Cognito.Config({
      Stage: config.STAGE,
      Domain: config.DOMAIN,
      InvitationEmailSubject: `Welcome To ${config.NICE_NAME}`,
      VerificationEmailSubject: `${config.NICE_NAME} requires your verification`,
      FromEmail: `verification@${config.NICE_NAME}`
    }),
    {
      type: 'shell',
      key: 'API',
      cwd: './api',
      command: 'make',
      args: ['configure'],
      outfile: '.envs.lambda',
    },
    Gen.Config()
  ]
}

export default configuration;
