import { useGlobals, useScriptRegistry, vartools } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';
import { useLam } from '@inf/cf-lam/config';
import { pathTransformer } from '@inf/core';

export const useLamApi = async (props: {
  _deprecatedAlias?: string,
  id: string,
  accountId: string,
  dependsOn?: string[],
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  MasterUsername: string,
  MasterUserPassword: string,
  apiHandler: {
    dbUrl: string,
    dbTestUrl: string,
    packageJsonPath: string,
    filepath: string,
    entrypoint: string,
  }
}) => {
  const { stage, currentModuleDir } = useGlobals();

  const transformPath = pathTransformer(__dirname, currentModuleDir);

  const result = await useLam({
    rootDir: __dirname,

    id: props.id,
    alias: props._deprecatedAlias,
    dependsOn: props.dependsOn && props.dependsOn.map(transformPath),
    handlers: {
      auth: {
        packageJsonPath: `${__dirname}/package.json`,
        filepath: `src/auth.ts`,
        entrypoint: 'handler',
        environment: { STAGE: stage }
      },
      api: {
        vars: {
          UserPoolId: props.cognitoUserPoolId,
          AWS_REGION: vars.AWS_REGION,
          DB_URL: props.apiHandler.dbUrl,
          DB_TEST_URL: props.apiHandler.dbTestUrl,
        },
        packageJsonPath: transformPath(props.apiHandler.packageJsonPath),
        filepath: props.apiHandler.filepath,
        entrypoint: props.apiHandler.entrypoint,
        environment: {
          STAGE: stage,
          MASTER_USERNAME: vartools.expand(props.MasterUsername),
          MASTER_USER_PASSWORD: vartools.expand(props.MasterUserPassword)
        },
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
      securityGroupIds: props.securityGroupIds,
      subnetIds: props.subnetIds,
    },
    slsIamRoleStatements: [{
      Effect: 'Allow',
      Resource: [`arn:aws:cognito-idp:${vars.AWS_REGION}:${props.accountId}:userpool/${props.cognitoUserPoolId}`],
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
        `arn:aws:s3:::uploads-${stage}-${vars.AWS_REGION}`,
        `arn:aws:s3:::uploads-${stage}-${vars.AWS_REGION}/*`
      ]
    }],
    webpackIgnore: /^pg-native$/,
    startCommands: [
      { command: 'yarn', args: ['-s', 'concurrently', '--kill-others', `"nodemon --watch ./build --exec 'yarn -s vars yarn -s serverless offline --host 0.0.0.0 --stage ${vars.STAGE} --skipCacheInvalidation true'"`, '"tsc -w -p tsconfig.sls.json"'] }
    ]
  });

  useScriptRegistry(props.id, {
    rules: {
      ['request.api']: {
        desc: 'makes a web request to api endpoint',
        commands: [{
          command: 'yarn',
          args: [
            '-s', 'vars', 'curl', '-s', '-H', `'Authorization: Bearer FIXME'`, '-H', `'Content-Type: application/json'`, '-d', '"hello world"',
            result.api]
        }]
      },
      ['request.guest']: {
        desc: 'makes a web request to guest api endpoint',
        commands: [{
          command: 'yarn',
          args: [
            '-s', 'vars', 'curl', '-s', '-H', `'Authorization: Guest'`, '-H', `'content-type: application/json'`, '-d',
            `'${JSON.stringify({
              query: 'query($arg: String!) { testUnauthorized (arg: $arg) }',
              variables: { arg: 'pong' }
            }).replace(/'/, '\'\'')}'`,
            result.api]
        }]
      }
    }
  });

  return result;
};