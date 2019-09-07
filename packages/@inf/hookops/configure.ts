import * as fs from 'fs';
import * as path from 'path';
import * as colors from 'colors/safe';
import { useClean } from './src/hooks/useclean';
import { Outputs, ConfigRecord, rootstate, createModule } from '.';

const error = (msg: string, id?: number) => console.error(`[CONF] ${colors.red(`error: ${msg}${id ? ` (${id})` : ''}`)}`);
const log = (msg: string) => console.log(`[CONF] ${colors.grey(msg)} `);

const main = async (event: string, verbose: boolean, force: boolean) => {

  rootstate.setForce(force);

  const configPath = 'configuration.ts';
  const configurationDir = path.dirname(path.resolve(configPath));
  if (!fs.existsSync(configPath)) {
    console.log('no configuration.ts found, exiting...');
    process.exit(1);
  }

  const { default: configuration } = await import('../../../configuration');

  if (!configuration.stage) {
    error('stage not set', 4);
    process.exit(1);
  }

  if (!configuration.stage.match(/[a-zA-Z0-9]+/)) {
    error('invalid stage value - must contain only alphanumeric characters', 5);
    process.exit(1);
  }

  if (event != 'up' && event != 'clean')
    throw new Error(`unsupported config event '${event}'`);

  let updated = false;

  const use: <R extends Outputs>(rec: ConfigRecord<R>) => Promise<R> = async <R extends Outputs>(rec: ConfigRecord<R>): Promise<R> => {

    const modulename = path.basename(rec.rootDir);

    if (rec.rootDir != __dirname)
      log(`${'  '.repeat(rootstate.rootDirStackLength() - 1)}${colors.grey(modulename)} `);

    rootConfiguration.currentRootDir = rootstate.rootDirPush(rec.rootDir);

    const ret = await rec.run(use);

    if (ret == undefined) {
      console.error(colors.red(`record match failed: ${JSON.stringify(ret, null, 2)}`), 15);
      process.exit(1);
    }

    rootConfiguration.currentRootDir = rootstate.rootDirPop();

    if (rec.rootDir != __dirname && updated)
      console.log(colors.green('(updated)'));

    return ret;
  };

  const rootConfiguration = rootstate.setConfig({
    verbose,
    force,
    currentRootDir: undefined as string | undefined,
    configurationDir,
    hookOpsDir: __dirname,
    stage: configuration.stage,
    use,
    markUpdated: () => updated = true
  });

  if (event == 'clean') {

    useClean().clean();

  } else if (event == 'up') {

    useClean([
      `${__dirname}/.inputs`,
      `${__dirname}/.cache`,
      `${__dirname}/.effects`
    ]);

    await use({
      rootDir: __dirname,
      run: async () => {
        await configuration.configure();
        return {};
      }
    });

  } else if (event == 'down') {
    error('down not yet implemented');
  } else {
    error(`unknown configure command '${event}'`, 16);
    throw new Error(`unknown configure command '${event}'`);
  }
};

const [_1, _2, ...rest] = process.argv;
let _verbose = false;
let _force = false;
const cmdargs = rest.filter(r => {
  if (r.startsWith('-')) {
    if (r == '-v' || r == '--verbose')
      _verbose = true;
    if (r == '-f' || r == '--force')
      _force = true;
    return false;
  } else {
    return true;
  }
});

if (cmdargs.length == 0) {
  console.log(`usage: configure <up|down>`);
  process.exit(1);
}

main(cmdargs[0], _verbose, _force).catch(err => {
  console.error(colors.red(err && err.stackpath || err), 17);
  process.exit(1);
});