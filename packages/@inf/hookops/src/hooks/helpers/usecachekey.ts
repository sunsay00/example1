import * as fs from 'fs';
import * as crypto from 'crypto';
import { useGlobals } from '../useglobals';
import * as path from 'path';

const _stack: { frameCount: number }[] = [{ frameCount: 0 }];

const lastmod = (filepath: string) => {
  if (fs.existsSync(filepath)) {
    const stats = fs.statSync(filepath);
    return new Date(stats.mtime).getTime();
  } else {
    return 0;
  }
}

const getCallerPath = () => {
  const stack = new Error().stack;
  if (stack) {
    const regex = /at (\/.+\.ts):\d+:\d+/gm;
    let match = regex.exec(stack);
    while (match != null) {
      if (match[1] != __filename)
        return match[1];
      match = regex.exec(stack);
    }
  }
  return '';
}

export const useCacheKey = async <T>(fn: (cachekey: string) => Promise<T>) => {
  const { configurationDir, currentModuleDir } = useGlobals();

  const relpath = path.relative(configurationDir, currentModuleDir);

  _stack[_stack.length - 1].frameCount = _stack[_stack.length - 1].frameCount + 1;
  _stack.push({ frameCount: 0 });

  const cachekey = `${relpath.replace(/\//g, '@')}@${lastmod(getCallerPath())}@${_stack.map(s => `${s.frameCount}`).join('_')}`;
  const shasum = crypto.createHash('sha1');
  shasum.update(cachekey);
  const hash = shasum.digest('hex');

  const result = await fn(hash);

  _stack.pop();

  return result;
}
