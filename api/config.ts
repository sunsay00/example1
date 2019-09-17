import { useLamApi } from '@inf/cf-lam-api/config';
import { createModule, useTempDir, useScriptRegistry } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';
import { useBackendGen, GenProps } from '@inf/cf-gen/config';
import { useVarsWriter, TunnelProps, useTunnel } from '@inf/hooks';

export const useApi = async (props: {
  accountId: string,
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  MasterUsername: string,
  MasterUserPassword: string,
  dbUrl: string,
  dbTestUrl: string,
  tunnelProps: TunnelProps,
  genProps: GenProps,
}) => createModule(async () => {

  const id = 'api';

  const tmpdir = useTempDir(id);

  const tunnel = useTunnel(props.tunnelProps);

  useScriptRegistry(id, {
    rules: {
      test: {
        cwd: tmpdir,
        desc: 'run tests',
        commands: [
          { command: 'yarn', args: ['-s', 'x', 'db', 'wipetest'] },
          tunnel({ command: 'yarn', args: ['-s', 'vars', 'yarn', '-s', 'test', '2>&1'] })
        ]
      }
    }
  });

  await useBackendGen({
    id,
    genProps: props.genProps
  });

  useVarsWriter('ts', __dirname, {
    DB_URL: props.dbUrl,
    DB_TEST_URL: props.dbTestUrl,
    AWS_REGION: vars.AWS_REGION,
    UserPoolId: props.cognitoUserPoolId
  });

  return await useLamApi({
    id,
    _deprecatedAlias: 'api',
    dependsOn: ['./package.json', './src/**/*.ts', './config.ts'],
    accountId: props.accountId,
    cognitoUserPoolId: props.cognitoUserPoolId,
    securityGroupIds: props.securityGroupIds,
    subnetIds: props.subnetIds,
    MasterUsername: props.MasterUsername,
    MasterUserPassword: props.MasterUserPassword,
    apiHandler: {
      dbUrl: props.dbUrl,
      dbTestUrl: props.dbTestUrl,
      packageJsonPath: './package.json',
      filepath: 'src/api.ts',
      entrypoint: 'handler',
    }
  });
})