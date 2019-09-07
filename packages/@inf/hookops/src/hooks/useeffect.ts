import * as fs from 'fs';
import { JSONArray } from '@inf/common';
import { useTempDir } from './usetempdir';
import { rootstate } from '..';
import { getJSONHash } from './usememo';
import { useCacheKey } from './helpers/usecachekey';

export const useEffect = async (fn: () => Promise<void>, changes?: JSONArray) => {
  if (!changes) {
    await fn();
  } else {
    await useCacheKey(async cachekey => {
      const tmpdir = useTempDir('effects');
      const hash = getJSONHash(changes);
      const hashpath = `${tmpdir}/${cachekey}`;

      if (
        rootstate.force() && changes.length == 0 ||
        !fs.existsSync(hashpath) ||
        fs.readFileSync(hashpath, { encoding: 'utf8' }) != hash
      ) {
        await fn();

        if (!fs.existsSync(tmpdir))
          fs.mkdirSync(tmpdir);

        fs.writeFileSync(hashpath, hash, { encoding: 'utf8' });
      }
    });
  }
}


