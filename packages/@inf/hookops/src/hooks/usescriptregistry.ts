import * as fs from 'fs';
import { entries } from '@inf/common';
import { useGlobals } from './useglobals';
import { useClean } from './useclean';
import { useUniqueIdAssertion } from './useuniqueidassertion';

type StdioOptions = 'pipe' | 'ignore' | 'inherit';

type Scripts = {
  cwd?: string,
  env?: { [_: string]: string },
  rules: {
    [_: string]: {
      stdio?: StdioOptions,
      cwd?: string,
      env?: { [_: string]: string }
      desc?: string,
      deps?: string[],
      commands?: {
        command: string,
        args?: string[]
      }[]
    }
  }
}

const _initialized: { [_: string]: boolean } = {};

export const useScriptRegistry = (id: string, scripts: Scripts) => {
  const { hookOpsDir, currentRootDir } = useGlobals();

  const missingVars = [] as string[];
  entries(scripts.rules).forEach(([k, v]) =>
    v.env && entries(v.env).forEach(([k, v]) =>
      v == undefined && missingVars.push(k)));
  if (missingVars.length > 0)
    throw new Error(`script-registry received undefined envvars [${missingVars.join(', ')}]`);

  entries(scripts.rules).forEach(([k, v]) => {
    useUniqueIdAssertion('script-registry', `${id}-${k.replace(/\./, '-DOT-')}`)
    if (v.commands) {
      v.commands.forEach(c => {
        if (!c.command)
          throw new Error(`invalid command found in script ${id} ${k}`);
        if (c.args) {
          c.args.forEach((a, i) => {
            if (!a)
              throw new Error(`invalid command argument detected for '${k} ${JSON.stringify(v)}`);
          });
        }
      });
    }
  });

  const tmpdir = `${hookOpsDir}/.scripts`;

  if (!fs.existsSync(tmpdir))
    fs.mkdirSync(tmpdir)

  const tmpfile = `${tmpdir}/${id}.json`;

  if (_initialized[tmpfile] && fs.existsSync(tmpfile)) {
    let prev: Scripts | undefined = undefined;
    try {
      prev = JSON.parse(fs.readFileSync(tmpfile, { encoding: 'utf8' }));
    } catch {
      console.warn(`corrupt script dectected '${tmpfile}', replacing`);
      const json = { ...scripts, cwd: scripts.cwd || currentRootDir };
      fs.writeFileSync(tmpfile, JSON.stringify(json, null, 2), { encoding: 'utf8' });
    }
    if (prev) {
      const env = prev.env && scripts.env && { ...prev.env, ...scripts.env };
      const cwd = scripts.cwd || prev.cwd || currentRootDir;
      const next = { ...scripts, cwd, env, rules: { ...prev.rules, ...scripts.rules } };
      fs.writeFileSync(tmpfile, JSON.stringify({ ...prev, ...next }, null, 2), { encoding: 'utf8' });
    }
  } else {
    const json = { ...scripts, cwd: scripts.cwd || currentRootDir };
    fs.writeFileSync(tmpfile, JSON.stringify(json, null, 2), { encoding: 'utf8' });
    _initialized[tmpfile] = true;
  }

  useClean([tmpdir]);
}
