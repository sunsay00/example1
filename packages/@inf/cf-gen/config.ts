import * as fs from 'fs';
import { createModule, useShell, useScriptRegistry, useGlobals } from '@inf/hookops';

const useDbProxy = (inputs: {
  db: {
    username: string,
    password: string,
    host: string,
    port: number
  },
  proxy: {
    host: string,
    port: number
  }
}) => {
  const { stage } = useGlobals();
  const host = stage == 'local' ? inputs.db.host : inputs.db.host;
  //const host = stage == 'local' ? inputs.db.host : 'localhost';
  const port = stage == 'local' ? inputs.db.port : inputs.db.port;
  return {
    dbUrl: `postgres://${inputs.db.username}:${inputs.db.password}@${host}:${port}/main${stage}`,
    dbTestUrl: `postgres://${inputs.db.username}:${inputs.db.password}@${host}:${port}/test${stage}`
  };
}

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
        desc: 'generates migrations, back-end-bindings, and front-end-bindings',
        commands: [{ command: 'make', args: ['configure'] }]
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
  db: {
    host: string,
    port: number,
  },
  migrations: {
    dir: string,
    databaseJsonPath: string,
  },
  proxy: {
    host: string,
    port: number
  }
}) => {
  const { stage } = useGlobals();

  const rdshost = stage != 'local' ? inputs.db.host : '';
  const proxyhost = stage != 'local' ? inputs.proxy.host : '';
  const localPort = stage != 'local' ? inputs.proxy.port : 0;

  const tunnelargs = stage == 'local' ? ['-s', 'vars'] : ['-s', 'tunnel', `${localPort}`, rdshost, `${inputs.db.port}`, proxyhost];

  await useShell({
    cwd: `${__dirname}/generator`,
    command: 'make',
    args: ['configure'],
    dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
    env: { LEDGER_PATH: inputs.ledgerPath }
  });

  await useShell({
    command: 'yarn',
    cwd: __dirname,
    args: [...tunnelargs, 'sh', 'seed', inputs.dbUrl, `${inputs.RDSServiceId}`],
    dependsOn: [`${__dirname}/generator/**/*.scm`, inputs.ledgerPath],
    env: { DB: inputs.dbUrl }
  });

  await useShell({
    command: 'yarn',
    cwd: __dirname,
    args: [...tunnelargs, 'yarn', '-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath],
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
          { command: 'yarn', args: [...tunnelargs, 'sh', 'seed', inputs.dbUrl, `${inputs.RDSServiceId}`] },
          { command: 'yarn', args: [...tunnelargs, 'yarn', '-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ]
      },
      wipetest: {
        env: {
          DB_URL: inputs.dbUrl,
          DB_TEST_URL: inputs.dbTestUrl,
          GENDIR: __dirname
        },
        desc: 'wipes test databases',
        commands: [{ command: 'yarn', args: [...tunnelargs, './wipetest', '2>&1'] }]
      },
      up: {
        env: { DB: inputs.dbUrl },
        desc: 'transitions the database to the lastest migration',
        commands: [
          { command: 'yarn', args: [...tunnelargs, 'yarn', '-s', 'db-migrate', 'up', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ]
      },
      down: {
        env: { DB: inputs.dbUrl },
        desc: 'transitions the database to its previous migration',
        commands: [
          { command: 'yarn', args: [...tunnelargs, 'yarn', '-s', 'db-migrate', 'down', '--migrations-dir', inputs.migrations.dir, '--config', inputs.migrations.databaseJsonPath] }
        ]
      },
      shell: {
        stdio: 'inherit',
        desc: 'opens a shell to the database',
        commands: [{ command: 'yarn', args: [...tunnelargs, 'pgcli', inputs.dbUrl] }]
      }
    }
  });

  return {};
};

export const useGen = (inputs: {
  db: {
    username: string,
    password: string,
    host: string,
    port: number
  },
  proxy: {
    host: string,
    port: number
  },
  RDSServiceId: number,
  ledgerPath: string
}) => createModule(__dirname, async () => {

  const { configurationDir } = useGlobals();

  const ledgerPath = inputs.ledgerPath.startsWith('/') ? inputs.ledgerPath : `${configurationDir}/${inputs.ledgerPath}`;
  if (!fs.existsSync(ledgerPath))
    throw new Error(`ledger file not found, looked in '${ledgerPath}'`);

  const proxy = useDbProxy({
    db: inputs.db,
    proxy: inputs.proxy
  });

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
    db: {
      host: inputs.db.host,
      port: inputs.db.port
    },
    ledgerPath,
    migrations: gen.migrations,
    proxy: inputs.proxy
  });

  return proxy;
});
