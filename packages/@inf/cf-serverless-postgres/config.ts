import { createModule, useGlobals } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';
import { useDocker } from '@inf/cf-docker/config';

export const usePostgres = (inputs: {
  DatabaseName: string,
  MasterUsername: string,
  MasterUserPassword: string,
  MinCapacity: number,
  MaxCapacity: number,
}) => createModule('cf-serverless-postgres', async () => {
  const { stage } = useGlobals();

  if (stage == 'local') {
    const postgres = await useDocker({
      id: 'postgres',
      service: {
        ports: ['5432:5432'],
        image: 'mdillon/postgis:9.6-alpine',
        environment: [
          `POSTGRES_USER=${inputs.MasterUsername}`,
          `POSTGRES_PASSWORD=${inputs.MasterUserPassword}`,
          `POSTGRES_DB=mainlocal`
        ]
      },
      outputs: { host: 'localhost' }
    });

    return {
      host: postgres.host,
      port: 5432
    };
  } else {
    const postgres = await useCloudFormation({
      id: 'cf-serverless-postgres',
      cfyamlpath: `${__dirname}/cf.yaml`,
      inputs: {
        DatabaseName: inputs.DatabaseName,
        MasterUsername: inputs.MasterUsername,
        MasterUserPassword: inputs.MasterUserPassword,
        MinCapacity: inputs.MinCapacity,
        MaxCapacity: inputs.MaxCapacity,
        Stage: stage
      },
      defaultOutputs: {
        RDSClusterEndpointAddress: '',
      }
    });

    return {
      host: postgres.RDSClusterEndpointAddress,
      port: 5432
    };
  }
});
