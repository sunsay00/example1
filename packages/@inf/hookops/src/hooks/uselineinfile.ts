import * as fs from 'fs';

export const useLineInFile = (filepath: string, linesToEnsure: string[]) => {
  if (linesToEnsure.length == 0) return;
  const ents = [...linesToEnsure].sort((a, b) => a.localeCompare(b));
  if (!fs.existsSync(filepath)) {
    fs.writeFileSync(filepath, ents.join('\n'), { encoding: 'utf8' });
  } else {
    const data = fs.readFileSync(filepath, { encoding: 'utf8' });
    const pents = data.split('\n').filter(e => !!e);
    const nents = [...pents, ...ents.filter(e => !pents.includes(e)).sort((a, b) => a.localeCompare(b))];
    fs.writeFileSync(filepath, nents.join('\n'), { encoding: 'utf8' });
  }
}