import { useLam } from '@inf/cf-lam/config';
import { createModule } from '@inf/hookops';

export const useLamTest = (inputs: {
  dbUrl: string
  securityGroupIds: string[],
  subnetIds: string[]
}) => createModule(async () => {
  return await useLam({
    rootDir: __dirname,
    id: 'test1',
    dependsOn: ['./package.json', './**/*.ts'],
    handlers: {
      test: {
        vars: {
          DB_URL: inputs.dbUrl,
        },
        packageJsonPath: './package.json',
        filepath: 'index.ts',
        entrypoint: 'handler'
      }
    },
    webpackIgnore: /^pg-native$/,
    sls: {
      vpc: {
        securityGroupIds: inputs.securityGroupIds,
        subnetIds: inputs.subnetIds
      }
    }
  });
});