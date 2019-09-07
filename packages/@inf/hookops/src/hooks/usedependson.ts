import * as fs from 'fs';
import * as colors from 'colors';
import * as path from 'path';
import * as mm from 'micromatch';
import { useCacheKey } from './helpers/usecachekey';
import { useGlobals } from './useglobals';

const forEachFile = (dir: string, opts: { glob: string, recurse: boolean }, continueFn: (name: string) => boolean): boolean => {
  const d = path.resolve(dir);
  const nodes = fs.readdirSync(d);
  let done = false;
  for (let n of nodes) {
    const p = d.endsWith('/') ? `${d}${n}` : `${d}/${n}`;
    if (fs.statSync(p).isDirectory()) {
      if (opts.recurse) {
        done = forEachFile(p, opts, continueFn);
        if (done)
          break;
      }
    } else {
      if (mm.isMatch(n, opts.glob)) {
        if (!continueFn(p)) {
          done = true;
          break;
        }
      }
    }
  }
  return done;
}

const lastmod = (filepath: string) => {
  if (fs.existsSync(filepath)) {
    const stats = fs.statSync(filepath);
    return new Date(stats.mtime).getTime();
  } else {
    return 0;
  }
}

const cacheExists = (key: string) =>
  fs.existsSync(`${__dirname}/.cache/${key}`);

const isAnyNewerThanCache = (key: string, dependsOn: string[]) => {
  if (!fs.existsSync(`${__dirname}/.cache/${key}`))
    return true;
  const t1 = lastmod(`${__dirname}/.cache/${key}`);
  let ret = false;
  for (let fullglob of dependsOn) {
    const split = fullglob.split('/');
    if (split.length == 0) continue;
    const recurse = split.length > 1 && (split[split.length - 2] == '**');
    const glob = split[split.length - 1];
    if (split.length > 0) split.pop();
    if (split.length > 0 && split[split.length - 1] == '**') split.pop();
    const fullpath = split.join('/');
    forEachFile(fullpath, { glob, recurse }, n => {
      const t2 = lastmod(n);
      if (t2 <= t1)
        return true;
      ret = true;
      return false;
    });
    if (ret)
      break;
  }
  return ret;
}

const writeCache = (key: string, data: {}) => {
  if (!fs.existsSync(`${__dirname}/.cache`))
    fs.mkdirSync(`${__dirname}/.cache`);
  const out = JSON.stringify(data, null, 2);
  fs.writeFileSync(`${__dirname}/.cache/${key}`, out);
};

export const useDependsOn = async (fn: () => Promise<void>, dependsOn?: string[]): Promise<void> => {
  const { configurationDir, markUpdated } = useGlobals();
  return await useCacheKey(async cachekey => {
    const absDependsOn = dependsOn && dependsOn.map(d => d.startsWith('/') ? d : path.join(configurationDir, d));

    let dirty = false;
    if (!absDependsOn) {
      dirty = true;
    } else {
      if (absDependsOn.length == 0) {
        if (!cacheExists(cachekey))
          dirty = true;
      } else {
        if (isAnyNewerThanCache(cachekey, absDependsOn))
          dirty = true;
      }
    }

    //const tmpfile = `${useTempDir('cache')}/${cachekey}`;
    //if (dirty || !fs.existsSync(tmpfile)) {
    //await fn();
    //fs.writeFileSync(tmpfile, '', { encoding: 'utf8' });
    //}

    if (dirty) {
      await fn();
      writeCache(cachekey, {});
      markUpdated();
    }
  });
}
