import { createConfig } from '@inf/vars/configure';
import * as Sls from '@inf/cf-sls/config';

export const Config = (inputs: {
  accountId: string,
  dependsOn?: string[],
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  apiHandler: {
    DB_URL: string,
    DB_TEST_URL: string,
    packageJsonPath: string,
    filepath: string,
    entrypoint: string,
  }
}) => {
  return createConfig(({ stage, region }) => Sls.Config({
    id: 'cf-sls-api',
    alias: 'api',

    rootDir: __dirname,

    dependsOn: inputs.dependsOn,

    vars: {
      UserPoolId: inputs.cognitoUserPoolId,
    },

    handlers: {
      auth: {
        packageJsonPath: `${__dirname}/package.json`,
        filepath: `src/auth.ts`,
        entrypoint: 'handler'
      },
      api: {
        vars: {
          UserPoolId: inputs.cognitoUserPoolId,
          AWS_REGION: region,
          DB_URL: inputs.apiHandler.DB_URL,
          DB_TEST_URL: inputs.apiHandler.DB_TEST_URL,
        },
        packageJsonPath: inputs.apiHandler.packageJsonPath,
        filepath: inputs.apiHandler.filepath,
        entrypoint: inputs.apiHandler.entrypoint,
        environment: { STAGE: stage },
        events: [{
          http: {
            authorizer: 'auth',
            path: 'graphql',
            method: 'post',
            cors: true
          }
        }]
      },
    },
    slsVpc: {
      securityGroupIds: inputs.securityGroupIds,
      subnetIds: inputs.subnetIds,
    },
    slsIamRoleStatements: [{
      Effect: 'Allow',
      Resource: [`arn:aws:cognito-idp:${region}:${inputs.accountId}:userpool/${inputs.cognitoUserPoolId}`],
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
        `arn:aws:s3:::uploads-${stage}-${region}`,
        `arn:aws:s3:::uploads-${stage}-${region}/*`
      ]
    }],
    webpackIgnore: /^pg-native$/,
    dockerServices: stage != 'local' ? undefined : {
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
    startCommands: [
      { command: 'yarn', args: ['-s', 'concurrently', '--kill-others', `"nodemon --watch ./build --exec 'yarn -s serverless offline --host 0.0.0.0 --stage {{STAGE}} --skipCacheInvalidation true'"`, '"tsc -w -p tsconfig.sls.json"'] }
    ]
  }))
};