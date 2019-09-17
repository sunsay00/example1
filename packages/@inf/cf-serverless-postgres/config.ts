import { createModule, useGlobals } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';
import { useDocker } from '@inf/cf-docker/config';

export const usePostgres = (props: {
  DatabaseName: string,
  MasterUsername: string,
  MasterUserPassword: string,
  MinCapacity: number,
  MaxCapacity: number,
}) => createModule(async () => {
  const { stage } = useGlobals();

  if (stage == 'local') {
    const postgres = await useDocker({
      id: 'postgres',
      service: {
        ports: ['5432:5432'],
        image: 'mdillon/postgis:9.6-alpine',
        environment: [
          `POSTGRES_USER=${props.MasterUsername}`,
          `POSTGRES_PASSWORD=${props.MasterUserPassword}`,
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
        DatabaseName: props.DatabaseName,
        MasterUsername: props.MasterUsername,
        MasterUserPassword: props.MasterUserPassword,
        MinCapacity: props.MinCapacity,
        MaxCapacity: props.MaxCapacity,
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
