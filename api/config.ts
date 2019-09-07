import { useLamApi } from '@inf/cf-lam-api/config';
import { vars } from '@inf/hookops/vars';
import { useVarsWriter } from '@inf/hooks';

export const useApi = async (inputs: {
  accountId: string,
  cognitoUserPoolId: string,
  securityGroupIds: string[],
  subnetIds: string[],
  MasterUsername: string,
  MasterUserPassword: string,
  dbUrl: string,
  dbTestUrl: string
}) => {

  useVarsWriter('ts', __dirname, {
    DB_URL: inputs.dbUrl,
    DB_TEST_URL: inputs.dbTestUrl,
    AWS_REGION: vars.AWS_REGION,
    UserPoolId: inputs.cognitoUserPoolId
  });

  return await useLamApi({
    rootDir: __dirname,
    id: 'api',
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
}