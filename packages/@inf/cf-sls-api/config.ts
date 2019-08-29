import * as Sls from '@inf/cf-sls/config';

type Inputs = {
  packageJsonPath: string,
  tsconfigJsonPath: string,

  handlers: { [_: string]: Sls.Handler },

  stage: string,
  region: string,
  accountId: string,
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
}

export const Config = (inputs: Inputs) => {
  const name = 'cf-sls-api';

  return Sls.Config({
    name,
    alias: 'api',

    packageJsonPath: inputs.packageJsonPath,
    tsconfigJsonPath: inputs.tsconfigJsonPath,

    handlers: inputs.handlers,

    stage: inputs.stage,
    region: inputs.region,
    slsVpc: {
      securityGroupIds: inputs.securityGroupIds,
      subnetIds: inputs.subnetIds,
    },
    slsIamRoleStatements: [{
      Effect: 'Allow',
      Resource: [`arn:aws:cognito-idp:${inputs.region}:${inputs.accountId}:userpool/${inputs.cognitoUserPoolId}`],
      Action: [
        'cognito-idp:AdminCreateUser',
        'cognito-idp:AdminDeleteUser',
        'cognito-idp:AdminConfirmSignUp',
        'cognito-idp:AdminAddUserToGroup',
        'cognito-idp:AdminDisableUser',
        'cognito-idp:AdminEnableUser',
        'cognito-idp:ListUsers',
        'cognito-idp:AdminUpdateUserAttributes',
        'cognito-idp:AdminGetUser'
      ]
    }, {
      Effect: 'Allow',
      Action: [
        's3:ListBucket',
        's3:PutObject',
        's3:PutObjectAcl',
        's3:AbortMultipartUpload',
        's3:GetObject',
        's3:GetObjectAcl',
        's3:ListBucketMultipartUploads',
        's3:ListMultipartUploadParts',
        's3:AbortMultipartUpload'
      ],
      Resource: [
        `arn:aws:s3:::uploads-${inputs.stage}-${inputs.region}`,
        `arn:aws:s3:::uploads-${inputs.stage}-${inputs.region}/*`
      ]
    }],
    webpackIgnore: /^pg-native$/,
    dockerServices: {
      [`redis_${name}`]: {
        container_name: `redis_${name}`,
        ports: ['6379:6379'],
        image: 'redis:3.2.10-alpine'
      },
      [`postgres_${name}`]: {
        container_name: `postgres_${name}`,
        ports: ['5432:5432'],
        image: 'mdillon/postgis:9.6-alpine',
        environment: [
          'POSTGRES_USER=root',
          'POSTGRES_PASSWORD=for-development-only',
          'POSTGRES_DB=mainlocal'
        ]
      }
    },
    packageDevDependencies: {
      "serverless-offline": "5.10.1",
    },
    slsPlugins: ['serverless-offline'],
    startCommands: [
      { command: 'yarn', args: ['-s', 'serverless', 'offline', '--host', '0.0.0.0', '--stage', '{{STAGE}}', '--skipCacheInvalidation', 'true'] }
    ]
  });
}