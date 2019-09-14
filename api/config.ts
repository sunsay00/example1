import { useLamApi } from '@inf/cf-lam-api/config';
import { createModule, useTempDir, useScriptRegistry, useGlobals } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';
import { useVarsWriter, Tunnel } from '@inf/hooks';

export const useApi = async (inputs: {
  accountId: string,
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  MasterUsername: string,
  MasterUserPassword: string,
  dbUrl: string,
  dbTestUrl: string,
  tunnel: Tunnel
}) => createModule('cf-lam-api', async () => {

  const id = 'api';

  const tmpdir = useTempDir(id);

  useScriptRegistry(id, {
    rules: {
      test: {
        cwd: tmpdir,
        desc: 'run tests',
        commands: [
          { command: 'yarn', args: ['-s', 'x', 'db', 'wipetest'] },
          inputs.tunnel({ command: 'yarn', args: ['-s', 'vars', 'yarn', '-s', 'test', '2>&1'] })
        ]
      }
    }
  });

  useVarsWriter('ts', __dirname, {
    DB_URL: inputs.dbUrl,
    DB_TEST_URL: inputs.dbTestUrl,
    AWS_REGION: vars.AWS_REGION,
    UserPoolId: inputs.cognitoUserPoolId
  });

  return await useLamApi({
    id,
    _deprecatedAlias: 'api',
    dependsOn: ['./package.json', './src/**/*.ts', './config.ts'],
    accountId: inputs.accountId,
    cognitoUserPoolId: inputs.cognitoUserPoolId,
    securityGroupIds: inputs.securityGroupIds,
    subnetIds: inputs.subnetIds,
    MasterUsername: inputs.MasterUsername,
    MasterUserPassword: inputs.MasterUserPassword,
    apiHandler: {
      dbUrl: inputs.dbUrl,
      dbTestUrl: inputs.dbTestUrl,
      packageJsonPath: './package.json',
      filepath: 'src/api.ts',
      entrypoint: 'handler',
    }
  });
})