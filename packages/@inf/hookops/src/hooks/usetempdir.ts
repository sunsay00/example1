import * as fs from 'fs';
import * as path from 'path';
import { useClean } from './useclean';
import { useGlobals } from './useglobals';

export const useTempDir = (key: string) => {
  const { hookOpsDir, currentModuleDir } = useGlobals();

  const tmprootdir = `${hookOpsDir}/.tmp`;

  if (!fs.existsSync(tmprootdir))
    fs.mkdirSync(tmprootdir)

  const tmpkeydir = `${tmprootdir}/${key}--${path.basename(currentModuleDir)}`;

  if (!fs.existsSync(tmpkeydir))
    fs.mkdirSync(tmpkeydir);

  useClean([tmprootdir]);

  return tmpkeydir;
}