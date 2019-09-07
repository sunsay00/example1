import * as fs from 'fs';
import * as crypto from 'crypto';
import { entries, JSONValue, JSONArray } from '@inf/common';
import { useTempDir } from './usetempdir';
import { useCacheKey } from './helpers/usecachekey';
import { rootstate } from '..';

export const getJSONHash = (data: JSONValue) => {
  const shasum = crypto.createHash('sha1');
  const recur = (x: JSONValue) => {
    if (x == null) {
      shasum.update('[null]');
    } else if (x instanceof Array) {
      shasum.update('[array]');
      x.forEach(i => recur(i));
    } else if (typeof x == 'object') {
      shasum.update('[object]');
      entries(x).sort((a, b) => a[0].localeCompare(b[0])).map(([k, v]) => {
        shasum.update(`[k:${k}]`);
        return [k, recur(v)];
      });
    } else {
      shasum.update(JSON.stringify(x));
    }
  };
  recur(data);
  return shasum.digest('hex');
}

export const useMemo = async <R extends JSONValue>(fn: () => Promise<R>, changes?: JSONArray): Promise<R> => {
  if (!changes) {
    return await fn();
  } else {
    return await useCacheKey(async cachekey => {
      const tmpdir = `${useTempDir('memo')}/${cachekey}`;
      const hashpath = `${tmpdir}/${getJSONHash(changes)}`;

      if (
        rootstate.force() && changes.length == 0 ||
        !fs.existsSync(hashpath)
      ) {
        const ret = await fn();

        if (!fs.existsSync(tmpdir))
          fs.mkdirSync(tmpdir);

        fs.writeFileSync(hashpath, JSON.stringify(ret, null, 2), { encoding: 'utf8' });
        return ret;
      } else {
        try {
          return JSON.parse(fs.readFileSync(hashpath, { encoding: 'utf8' }));
        } catch (err) {
          const ret = await fn();

          if (!fs.existsSync(tmpdir))
            fs.mkdirSync(tmpdir);

          fs.writeFileSync(hashpath, JSON.stringify(ret, null, 2), { encoding: 'utf8' });
          return ret;
        }
      }
    });
  }
}

