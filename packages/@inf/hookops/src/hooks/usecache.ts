import * as fs from 'fs';
import * as colors from 'colors';
import { JSONValue } from '@inf/common';
import { useTempDir } from './usetempdir';
import { useGlobals } from '..';
import { useCacheKey } from './helpers/usecachekey';

export const useCache = async <T extends JSONValue>(fn: () => Promise<T>, forceUpdate: boolean = false) => {
  return await useCacheKey(async cachekey => {
    const { verbose, markUpdated } = useGlobals();
    const tmpfile = `${useTempDir('cache')}/${cachekey}`;
    if (forceUpdate || !fs.existsSync(tmpfile)) {
      const ret = await fn();
      markUpdated();
      fs.writeFileSync(tmpfile, JSON.stringify(ret, null, 2), { encoding: 'utf8' });
      if (verbose) {
        const msg = JSON.stringify(ret, null, 2);
        if (msg != '{}')
          console.log(`${colors.blue(`cache:`)} ${colors.grey(msg)}`);
      }
      return ret;
    } else {
      try {
        const data = fs.readFileSync(tmpfile, { encoding: 'utf8' });
        return JSON.parse(data);
      } catch (err) {
        console.log(colors.red(err));
        const ret = await fn();
        markUpdated();
        fs.writeFileSync(tmpfile, JSON.stringify(ret, null, 2), { encoding: 'utf8' });
        if (verbose) {
          const msg = JSON.stringify(ret, null, 2);
          if (msg != '{}')
            console.log(`${colors.blue(`cache:`)} ${colors.grey(msg)}`);
        }
        return ret;
      }
    }
  });
}
