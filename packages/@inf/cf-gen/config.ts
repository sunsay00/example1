import * as fs from 'fs';
import { createModule, useShell, useScriptRegistry, useGlobals } from '@inf/hookops';
import { Tunnel } from '@inf/hooks';

//const useDbProxy = (inputs: {
//db: { username: string, password: string, host: string, port: number }, proxy: { host: string, port: number }
//}) => {
//const { stage } = useGlobals();
//const host = stage == 'local' ? inputs.db.host : 'localhost';
//const port = stage == 'local' ? inputs.db.port : inputs.proxy.port;
//return {
//dbUrl: `postgres://${inputs.db.username}:${inputs.db.password}@${host}:${port}/main${stage}`,
//dbTestUrl: `postgres://${inputs.db.username}:${inputs.db.password}@${host}:${port}/test${stage}`
//};
//}

const useGenerator = async (inputs: {
  dbUrl: string,
  dbTestUrl: string,
  RDSServiceId: number,
  RDSClusterEndpointAddress: string,
  ledgerPath: string
}) => {
  useScriptRegistry('gen', {
    cwd: __dirname,
    rules: {
      generate: {
        cwd: `${__dirname}/generator`,
        env: { LEDGER_PATH: inputs.ledgerPath },
        desc: 'generates migrations, and backend-bindings',
        commands: [{ command: 'make', args: ['configure'] }]
      },
      generateclient: {
        cwd: `${__dirname}/generator`,
        env: { LEDGER_PATH: inputs.ledgerPath },
        desc: 'generates frontend-bindings',
        commands: [{ command: 'make', args: ['OUTPUT_PATH={{flags:outdir}}', 'configure.client'] }],
        flags: {
          outdir: {
            desc: 'output directory',
            shortcut: 'o'
          }
        }
      }
    }
  });

  return {
    migrations: {
      dir: `${__dirname}/infra/postgres/migrations`,
      databaseJsonPath: `${__dirname}/database.json`
    }
  };
}

const useMigrator = async (inputs: {
  ledgerPath: string,
  RDSServiceId: number,
  dbUrl: string,
  dbTestUrl: string,
  migrations: {
    dir: string,
    databaseJsonPath: string,
  },
  tunnel: Tunnel
}) => {

  await useShell({
    cwd: `${__dirname}/generator`,
    command: 'make',
    args: ['configure'],
    dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
    env: { LEDGER_PATH: inputs.ledgerPath }
  });

  await useShell({
    ...inputs.tunnel({
      command: 'sh',
      args: ['seed', inputs.dbUrl, `${inputs.RDSServiceId}`]
    }),
    cwd: __dirname,
    dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
    env: { DB: inputs.dbUrl }
  });

  await useShell({
    ...inputs.tunnel({
      command: 'yarn',
      args: ['-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath]
    }),
    cwd: __dirname,
    dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
    env: { DB: inputs.dbUrl }
  });

  useScriptRegistry('db', {
    cwd: __dirname,
    rules: {
      seed: {
        env: { DB: inputs.dbUrl },
        desc: 'seed the database',
        commands: [
          { command: 'sh', args: ['seed', inputs.dbUrl, `${inputs.RDSServiceId}`] },
          { command: 'yarn', args: ['-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ].map(inputs.tunnel)
      },
      wipetest: {
        env: {
          DB_URL: inputs.dbUrl,
          DB_TEST_URL: inputs.dbTestUrl,
          GENDIR: __dirname
        },
        desc: 'wipes test databases',
        commands: [
          { command: './wipetest', args: ['2>&1'] }
        ].map(inputs.tunnel)
      },
      up: {
        env: { DB: inputs.dbUrl },
        desc: 'transitions the database to the lastest migration',
        commands: [
          { command: 'yarn', args: ['-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ].map(inputs.tunnel)
      },
      down: {
        env: { DB: inputs.dbUrl },
        desc: 'transitions the database to its previous migration',
        commands: [
          { command: 'yarn', args: ['-s', 'db-migrate', 'down', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ].map(inputs.tunnel)
      },
      shell: {
        stdio: 'inherit',
        desc: 'opens a shell to the database',
        commands: [
          { command: 'pgcli', args: [inputs.dbUrl] }
        ].map(inputs.tunnel)
      }
    }
  });

  return {};
};

export const useGen = (inputs: {
  username: string,
  password: string,
  db: {
    host: string,
    port: number
  },
  proxy: {
    host: string,
    port: number
  },
  RDSServiceId: number,
  ledgerPath: string,
  tunnel: Tunnel
}) => createModule(async () => {
  const { stage, configurationDir } = useGlobals();

  const ledgerPath = inputs.ledgerPath.startsWith('/') ? inputs.ledgerPath : `${configurationDir}/${inputs.ledgerPath}`;
  if (!fs.existsSync(ledgerPath))
    throw new Error(`ledger file not found, looked in '${ledgerPath}'`);

  const proxyhost = stage == 'local' ? inputs.db.host : 'localhost';
  const proxyport = stage == 'local' ? inputs.db.port : inputs.proxy.port;
  const proxy = {
    dbUrl: `postgres://${inputs.username}:${inputs.password}@${proxyhost}:${proxyport}/main${stage}`,
    dbTestUrl: `postgres://${inputs.username}:${inputs.password}@${proxyhost}:${proxyport}/test${stage}`
  };

  const gen = await useGenerator({
    dbUrl: proxy.dbUrl,
    dbTestUrl: proxy.dbTestUrl,
    RDSServiceId: inputs.RDSServiceId,
    RDSClusterEndpointAddress: inputs.db.host,
    ledgerPath,
  });

  await useMigrator({
    dbUrl: proxy.dbUrl,
    dbTestUrl: proxy.dbTestUrl,
    RDSServiceId: inputs.RDSServiceId,
    ledgerPath,
    migrations: gen.migrations,
    tunnel: inputs.tunnel
  });

  return {
    dbUrl: `postgres://${inputs.username}:${inputs.password}@${inputs.db.host}:${inputs.db.port}/main${stage}`,
    dbTestUrl: `postgres://${inputs.username}:${inputs.password}@${inputs.db.host}:${inputs.db.port}/test${stage}`
  };
});
