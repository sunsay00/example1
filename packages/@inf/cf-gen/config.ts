import * as fs from 'fs';
import { createModule, useShell, useScriptRegistry, useGlobals, useClean } from '@inf/hookops';
import { useGitIgnore, TunnelProps, useTunnel } from '@inf/hooks';

export type GenProps = {
  ledgerPath: string,
  RDSServiceId: number,
  dbUrl: string,
  dbTestUrl: string,
  tunnelProps: TunnelProps
};

const useGenerator = async (props: {
  dbUrl: string,
  dbTestUrl: string,
  RDSServiceId: number,
  RDSClusterEndpointAddress: string,
  ledgerPath: string
}) => {
  useScriptRegistry('gen', {
    cwd: __dirname,
    rules: {
      backend: {
        cwd: `${__dirname}/generator`,
        env: { LEDGER_PATH: props.ledgerPath },
        desc: 'generates backend-bindings',
        commands: [{ command: 'make', args: ['OUTPUT_PATH={{flags:outdir}}', 'backend'] }],
        flags: {
          outdir: {
            desc: 'output directory',
            shortcut: 'o'
          }
        }
      },
      frontend: {
        cwd: `${__dirname}/generator`,
        env: { LEDGER_PATH: props.ledgerPath },
        desc: 'generates frontend-bindings',
        commands: [{ command: 'make', args: ['OUTPUT_PATH={{flags:outdir}}', 'frontend'] }],
        flags: {
          outdir: {
            desc: 'output directory',
            shortcut: 'o'
          }
        }
      }
    }
  });
}

const useMigrator = async (props: {
  id: string,
  ledgerPath: string,
  RDSServiceId: number,
  dbUrl: string,
  dbTestUrl: string,
  migrations: {
    dir: string,
    databaseJsonPath: string,
  },
  tunnelProps: TunnelProps
}) => {
  const { currentModuleDir } = useGlobals();

  const tunnel = useTunnel(props.tunnelProps);

  await useShell({
    command: 'yarn',
    dependsOn: [`${__dirname}/generator/**/*.scm`, props.ledgerPath],
    args: ['-s', 'x', 'gen', 'backend', '-o', `${currentModuleDir}/src/_gen`]
  });

  await useShell({
    ...tunnel({
      command: 'sh',
      args: ['seed', props.dbUrl, `${props.RDSServiceId}`]
    }),
    cwd: __dirname,
    dependsOn: [`${__dirname}/generator/**/*.scm`, props.ledgerPath],
    env: { DB: props.dbUrl }
  });

  await useShell({
    ...tunnel({
      command: 'yarn',
      args: ['-s', 'db-migrate', 'up', '--migrations-dir', props.migrations.dir, '--config', props.migrations.databaseJsonPath]
    }),
    cwd: __dirname,
    dependsOn: [`${__dirname}/generator/**/*.scm`, props.ledgerPath],
    env: { DB: props.dbUrl }
  });

  useScriptRegistry(`${props.id}-db`, {
    cwd: __dirname,
    rules: {
      seed: {
        env: { DB: props.dbUrl },
        desc: 'seed the database',
        commands: [
          { command: 'sh', args: ['seed', props.dbUrl, `${props.RDSServiceId}`] },
          { command: 'yarn', args: ['-s', 'db-migrate', 'up', '--migrations-dir', props.migrations.dir, '--config', props.migrations.databaseJsonPath] }
        ].map(tunnel)
      },
      wipetest: {
        env: {
          DB_URL: props.dbUrl,
          DB_TEST_URL: props.dbTestUrl,
          GENDIR: __dirname,
          DATABASE_JSON_PATH: props.migrations.databaseJsonPath
        },
        desc: 'wipes test databases',
        commands: [
          { command: './wipetest', args: ['2>&1'] }
        ].map(tunnel)
      },
      up: {
        env: { DB: props.dbUrl },
        desc: 'transitions the database to the lastest migration',
        commands: [
          { command: 'yarn', args: ['-s', 'db-migrate', 'up', '--migrations-dir', props.migrations.dir, '--config', props.migrations.databaseJsonPath] }
        ].map(tunnel)
      },
      down: {
        env: { DB: props.dbUrl },
        desc: 'transitions the database to its previous migration',
        commands: [
          { command: 'yarn', args: ['-s', 'db-migrate', 'down', '--migrations-dir', props.migrations.dir, '--config', props.migrations.databaseJsonPath] }
        ].map(tunnel)
      },
      shell: {
        stdio: 'inherit',
        desc: 'opens a shell to the database',
        commands: [
          { command: 'pgcli', args: [props.dbUrl] }
        ].map(tunnel)
      }
    }
  });

  return {};
};

export const useGen = (props: {
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
  tunnelProps: TunnelProps
}) => createModule(async () => {
  const { stage, configurationDir } = useGlobals();

  const ledgerPath = props.ledgerPath.startsWith('/') ? props.ledgerPath : `${configurationDir}/${props.ledgerPath}`;
  if (!fs.existsSync(ledgerPath))
    throw new Error(`ledger file not found, looked in '${ledgerPath}'`);

  const proxyhost = stage == 'local' ? props.db.host : 'localhost';
  const proxyport = stage == 'local' ? props.db.port : props.proxy.port;
  const proxy = {
    dbUrl: `postgres://${props.username}:${props.password}@${proxyhost}:${proxyport}/main${stage}`,
    dbTestUrl: `postgres://${props.username}:${props.password}@${proxyhost}:${proxyport}/test${stage}`
  };

  await useGenerator({
    dbUrl: proxy.dbUrl,
    dbTestUrl: proxy.dbTestUrl,
    RDSServiceId: props.RDSServiceId,
    RDSClusterEndpointAddress: props.db.host,
    ledgerPath,
  });

  return {
    props: {
      dbUrl: proxy.dbUrl,
      dbTestUrl: proxy.dbTestUrl,
      RDSServiceId: props.RDSServiceId,
      ledgerPath,
      tunnelProps: props.tunnelProps
    },
    dbUrl: `postgres://${props.username}:${props.password}@${props.db.host}:${props.db.port}/main${stage}`,
    dbTestUrl: `postgres://${props.username}:${props.password}@${props.db.host}:${props.db.port}/test${stage}`
  };
});

export const useFrontendGen = async (props: {
  genProps: GenProps
}) => {
  const { configurationDir, currentModuleDir } = useGlobals();

  const ledgerPath = props.genProps.ledgerPath.startsWith('/') ? props.genProps.ledgerPath : `${configurationDir}/${props.genProps.ledgerPath}`;
  if (!fs.existsSync(ledgerPath))
    throw new Error(`ledger file not found, looked in '${ledgerPath}'`);

  await useShell({
    command: 'yarn',
    dependsOn: [`${__dirname}/generator/**/*.scm`, ledgerPath],
    args: ['-s', 'x', 'gen', 'frontend', '-o', `${currentModuleDir}/src/_gen`]
  });

  useGitIgnore(currentModuleDir, ['src/_gen']);

  useClean([`${currentModuleDir}/src/_gen`])
}

export const useBackendGen = async (props: {
  id: string,
  genProps: GenProps
}) => {
  const { configurationDir, currentModuleDir } = useGlobals();

  const ledgerPath = props.genProps.ledgerPath.startsWith('/') ? props.genProps.ledgerPath : `${configurationDir}/${props.genProps.ledgerPath}`;
  if (!fs.existsSync(ledgerPath))
    throw new Error(`ledger file not found, looked in '${ledgerPath}'`);

  await useMigrator({
    ...props.genProps,
    id: props.id,
    migrations: {
      dir: `${currentModuleDir}/src/_gen/infra/postgres/migrations`,
      databaseJsonPath: `${currentModuleDir}/src/_gen/database.json`
    }
  });

  await useShell({
    command: 'yarn',
    dependsOn: [`${__dirname}/generator/**/*.scm`, ledgerPath],
    args: ['-s', 'x', 'gen', 'backend', '-o', `${currentModuleDir}/src/_gen`]
  });

  useGitIgnore(currentModuleDir, ['src/_gen']);

  useClean([`${currentModuleDir}/src/_gen`])
}