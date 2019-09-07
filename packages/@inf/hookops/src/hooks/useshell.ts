import { spawn } from 'child_process';
import * as colors from 'colors/safe';
import * as path from 'path';
import { fromEntries, entries } from '@inf/common';
import { useDependsOn } from './usedependson';
import { useCache } from './usecache';
import { useGlobals } from './useglobals';
import { vartools } from '..';

export const useShell = async <R>(inputs: {
  command: string,
  args: string[],
  dependsOn?: string[],
  env?: { [_: string]: string },
  cwd?: string,
  outputMatchers?: { [_ in keyof R]: RegExp }
}) => {
  const { verbose } = useGlobals();

  let dirty = false;

  await useDependsOn(async () => {
    dirty = true;
  }, inputs.dependsOn);

  return await useCache(async () => {
    return await new Promise<{ [_ in keyof R]: string }>((resolve, reject) => {
      const cwdEnabled = !inputs.command.startsWith('.') && !inputs.command.startsWith('/') && inputs.cwd;
      const cwd2 = cwdEnabled && inputs.cwd ? inputs.cwd : path.dirname(inputs.command);
      const cmd = cwdEnabled ? inputs.command : path.basename(inputs.command);
      verbose && console.log(colors.gray(`${colors.blue('shell:')} ${cmd} ${inputs.args.join(' ')} (cwd=${cwd2})`));
      const args = inputs.args.map(vartools.expand);
      const env = { ...process.env, ...fromEntries(entries(inputs.env || {}).map(([k, s]) => [k, vartools.expand(s)])) };
      const proc = spawn(cmd, args, { env, cwd: cwd2 });
      let buffer = '';
      proc.stdout.on('data', data => {
        const buf = data.toString();
        if (verbose)
          process.stdout.write(colors.gray(buf));
        buffer += buf;
      });
      proc.stderr.on('data', data => {
        const buf = data.toString();
        if (verbose)
          process.stderr.write(colors.gray(buf));
        buffer += buffer;
      });
      proc.on('close', code => {
        if (code != 0) {
          reject(new Error(`shell failed - exited with ${code}`));
        } else {
          const json: Partial<{ [_ in keyof R]: string }> = {};
          if (inputs.outputMatchers) {
            entries(inputs.outputMatchers).forEach(([k, v]) => {
              const match = v.exec(buffer);
              if (!match || match.length <= 1)
                throw new Error(`output for '${k}' had no match`);
              json[k] = match[1];
            });
          }
          resolve(json as { [_ in keyof R]: string });
        }
      });
    });
  }, dirty);
}
