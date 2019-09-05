import * as path from 'path';
import * as fs from 'fs';

export * from './serverless/apiwrapper';
export * from './serverless/authwrapper';
export * from './serverless/domainwrapper';

export const unlinkRecursiveSync = (p: string) => {
  if (fs.lstatSync(p).isDirectory()) {
    fs.readdirSync(p).forEach(f => unlinkRecursiveSync(path.join(p, f)));
    fs.rmdirSync(p);
  } else {
    fs.unlinkSync(p);
  }
};

export * from './maketools';