import { spawn } from 'child_process';
import * as colors from 'colors/safe';
import * as path from 'path';
import { fromEntries, entries } from '@inf/common';
import { useDependsOn } from './usedependson';
import { useCache } from './usecache';
import { useGlobals } from './useglobals';
import { vartools } from '..';

export const useShell = async <R>(props: {
  command: string,
  args: string[],
  dependsOn?: string[],
  env?: { [_: string]: string },
  cwd?: string,
  outputMatchers?: { [_ in keyof R]: RegExp }
}): Promise<{ [_ in keyof R]: string }> => {
  const { verbose } = useGlobals();

  let dirty = false;

  await useDependsOn(async () => {
    dirty = true;
  }, props.dependsOn);

  return await useCache(async () => {
    return await new Promise<{ [_ in keyof R]: string }>((resolve, reject) => {
      const cwdEnabled = !props.command.startsWith('.') && !props.command.startsWith('/') && props.cwd;
      const cwd2 = cwdEnabled && props.cwd ? props.cwd : path.dirname(props.command);
      const cmd = cwdEnabled ? props.command : path.basename(props.command);
      verbose && console.log(colors.gray(`${colors.blue('shell:')} ${cmd} ${props.args.join(' ')} (cwd=${cwd2})`));
      const args = props.args.map(vartools.expand);
      const env = { ...process.env, ...fromEntries(entries(props.env || {}).map(([k, s]) => [k, vartools.expand(s)])) };
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
          if (props.outputMatchers) {
            entries(props.outputMatchers).forEach(([k, v]) => {
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
