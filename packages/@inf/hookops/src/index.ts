import * as fs from 'fs';
import * as path from 'path';
import { useGlobals } from './hooks/useglobals';
import { useUniqueIdAssertion } from './hooks/useuniqueidassertion';

export type Outputs = { [_: string]: string | number | boolean };
export type ConfigRecord<R extends Outputs> = { rootDir: string, run: (use: UseFn) => Promise<R> };
type UseFn = <R extends Outputs>(record: ConfigRecord<R>) => Promise<R>;

export type Configuration = {
  stage: string;
  configure: () => Promise<void>
};

const _rootstate = {
  force: false,
  config: undefined as {
    verbose: boolean,
    force: boolean,
    currentModuleDir: string | undefined,
    configurationDir: string,
    hookOpsDir: string,
    stage: string,
    use: (<R extends Outputs>(rec: ConfigRecord<R>) => Promise<R>) | undefined,
    markUpdated: () => void
  } | undefined,
  dirstack: [] as string[],
  fqids: {} as { [_: string]: boolean }
};

const getCallerRootPath = (stack: string | undefined) => {
  if (stack) {
    const regex = /at [^/)]+(\/.+\.(t|j)sx?):\d+:\d+/gm;
    let match = regex.exec(stack);
    let i = 0;
    while (match != null) {
      if (i == 1)
        return match[1];
      match = regex.exec(stack);
      ++i;
    }
  }
  return '';
}

export const createModule = <R extends Outputs>(moduleid: string, run: () => Promise<R>) => {
  const stack = new Error().stack;
  const rootDir = path.dirname(getCallerRootPath(stack));
  useUniqueIdAssertion('module', moduleid);
  if (!fs.existsSync(`${rootDir}/package.json`))
    throw new Error('createModule may only be invoked in a script that resides in a directory that contains a package.json file');
  if (!_rootstate.config || !_rootstate.config.use)
    throw new Error('createModule must be invoked within a configuration context');
  return _rootstate.config.use(({ rootDir, run }));
}

export const rootstate = {
  force: () => _rootstate.force,
  setForce: (f: boolean) => _rootstate.force = f,
  setConfig: (config: typeof _rootstate['config']) => _rootstate.config = config,
  config: () => _rootstate.config,
  rootDirPush: (rootDir: string) => {
    _rootstate.dirstack.push(path.resolve(rootDir));
    return _rootstate.dirstack[_rootstate.dirstack.length - 1];
  },
  rootDirPop: () => {
    _rootstate.dirstack.pop();
    return _rootstate.dirstack.length == 0 ? undefined : _rootstate.dirstack[_rootstate.dirstack.length - 1];
  },
  rootDirStackLength: () => _rootstate.dirstack.length,
  fqIdDeclare: (fqid: string) => {
    if (_rootstate.fqids[fqid])
      throw new Error(`duplicate uniqueid '${fqid}' detected`);
    _rootstate.fqids[fqid] = true;
  }
};

export { vartools } from './vartools';

export { useGlobals } from './hooks/useglobals';
export { useUniqueIdAssertion } from './hooks/useuniqueidassertion';
export { useMemo } from './hooks/usememo';
export { useEffect } from './hooks/useeffect';
export { useClean } from './hooks/useclean';
export { useTempDir } from './hooks/usetempdir';
export { useShell } from './hooks/useshell';
export { useDependsOn } from './hooks/usedependson';
export { useLineInFile } from './hooks/uselineinfile';
export { useScriptRegistry } from './hooks/usescriptregistry';
export { useCache } from './hooks/usecache';