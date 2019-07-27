import { createConfig } from 'common';

const config = createConfig({
  AWS_REGION: process.env.AWS_REGION,
  STAGE: process.env.STAGE,
  MASTER_USERNAME: process.env.MASTER_USERNAME,
  MASTER_USER_PASSWORD: process.env.MASTER_USER_PASSWORD,
  DOMAIN: process.env.DOMAIN,
  NICE_NAME: process.env.NICE_NAME
});

export default {
  region: config.AWS_REGION,
  stage: config.STAGE,
  modules: [
    {
      type: 'cloudformation',
      name: 'cf-serverless-postgres',
      key: 'SLSPG',
      inputs: {
        Stage: config.STAGE,
        DatabaseName: 'main',
        MasterUsername: config.MASTER_USERNAME,
        MasterUserPassword: config.MASTER_USER_PASSWORD,
        MinCapacity: 2,
        MaxCapacity: 2
      }
    },
    /*{
      name: 'cf-cert',
      inputs: {
        Domain: config.DOMAIN
      },
      outputs: ['CertificateArn']
    },*/
    {
      type: 'cloudformation',
      name: 'cf-cognito',
      key: 'COG',
      inputs: {
        Stage: config.STAGE,
        Domain: config.DOMAIN,
        InvitationEmailSubject: `Welcome To ${config.NICE_NAME}`,
        VerificationEmailSubject: `${config.NICE_NAME} requires your verification`,
        FromEmail: `verification@${config.DOMAIN}`
      },
      outputs: ['IdentityPoolId', 'UserPoolId', 'WebUserPoolClientId', 'MobileUserPoolClientId']
    },
    {
      type: 'shell',
      key: 'API',
      rootDir: './api',
      command: 'make configure'
    }
  ]
}
