import *  as path from 'path';
import * as fs from 'fs';
import { unlinkRecursiveSync } from '@inf/core';

let _cleanerclear: { [_: string]: boolean } | undefined = undefined;
export const useClean = (paths?: string[]) => {

  const trashpath = path.resolve(`/tmp/.hookops.trash`);

  paths && paths.forEach(p => {
    const abs = path.resolve(p);
    if (_cleanerclear == undefined) {
      //if (fs.existsSync(trashpath))
      //fs.unlinkSync(trashpath);
      _cleanerclear = { [abs]: true };
      fs.appendFileSync(trashpath, `${path.resolve(p)}\n`);
    } else {
      if (!_cleanerclear[abs]) {
        fs.appendFileSync(trashpath, `${path.resolve(p)}\n`);
        _cleanerclear[abs] = true;
      }
    }
  });

  const clean = () => {
    if (!fs.existsSync(trashpath)) return;
    const paths = fs.readFileSync(trashpath, { encoding: 'utf8' }).split('\n');
    for (let p of paths) {
      if (fs.existsSync(p)) {
        if (
          p.match(/.+\/\.tmp\/?$/) ||
          p.match(/.+\/\.scripts\/?$/) ||
          p.match(/.+\/tmp\/?$/) ||
          p.match(/.+\/_gen\./) ||
          p.match(/.+\/_vars\./) ||
          p.match(/.+\/_outputs\./) ||
          p.match(/.+\/\.inputs\/?$/) ||
          p.match(/.+\/\.cache\/?$/) ||
          p.match(/.+\/\.effects\/?$/)) {
          const stat = fs.statSync(p);
          if (stat.isDirectory()) {
            console.log(`deleted: ${p}`);
            unlinkRecursiveSync(p);
          } else if (stat.isFile()) {
            console.log(`deleted: ${p}`);
            fs.unlinkSync(p);
          }
        }
      }
    }
    fs.unlinkSync(trashpath);
  };

  return { clean };
}

