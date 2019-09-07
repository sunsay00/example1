import { createConfig, makeStackname, expandVariables } from '@inf/vars/configure';
import * as Lam from '@inf/cf-lam/config';

export const Config = (inputs: {
  id: string,
  accountId: string,
  dependsOn?: string[],
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  MasterUsername: string,
  MasterUserPassword: string,
  apiHandler: {
    DB_URL: string,
    DB_TEST_URL: string,
    packageJsonPath: string,
    filepath: string,
    entrypoint: string,
  }
}) => {
  const localname = makeStackname('', __dirname, inputs.id);
  return createConfig(({ stage, region }) => Lam.Config({
    id: inputs.id,
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
    lamVpc: {
      securityGroupIds: inputs.securityGroupIds,
      subnetIds: inputs.subnetIds,
    },
    lamIamRoleStatements: [{
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
      [`redis_${localname}`]: {
        container_name: `redis_${localname}`,
        ports: ['6379:6379'],
        image: 'redis:3.2.10-alpine'
      },
      [`postgres_${localname}`]: {
        container_name: `postgres_${localname}`,
        ports: ['5432:5432'],
        image: 'mdillon/postgis:9.6-alpine',
        environment: [
          `POSTGRES_USER=${inputs.MasterUsername}`,
          `POSTGRES_PASSWORD=${inputs.MasterUserPassword}`,
          `POSTGRES_DB=mainlocal`
        ].map(expandVariables)
      }
    },
    startCommands: [
      { command: 'yarn', args: ['-s', 'concurrently', '--kill-others', `"nodemon --watch ./build --exec 'yarn -s serverless offline --host 0.0.0.0 --stage {{STAGE}} --skipCacheInvalidation true'"`, '"tsc -w -p tsconfig.sls.json"'] }
    ],
    makeRules: {
      ['request.api']: {
        desc: 'makes a web request to api endpoint',
        commands: [`yarn -s vars curl -H 'Authorization: Bearer FIXME' -H 'Content-Type: application/json' -d "hello world" {{${inputs.id.toUpperCase().replace(/-/g, '_')}_ApiEndpoint}}`]
      },
      ['request.guest']: {
        desc: 'makes a web request to guest api endpoint',
        commands: [`yarn -s vars curl -H 'Authorization: Guest' -H 'content-type: application/json' -d '{"query":"query($$arg: String!) { testUnauthorized (arg: $$arg) }","variables": { "arg": "pong" }}' {{${inputs.id.toUpperCase().replace(/-/g, '_')}_ApiEndpoint}}`]
      }
    }
  }));
}