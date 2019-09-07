import * as fs from 'fs';
import * as colors from 'colors/safe';
import { entries } from '@inf/common';
import { } from '@inf/core';
import { Outputs, useGlobals, useClean, vartools } from '@inf/hookops';
import { useGitIgnore } from './usegitignore';

const toTsVarsString = (data: Outputs) => {
  let out = '';
  Object.entries(data).sort((a, b) => a[0].localeCompare(b[0])).map(([k, v]) => {
    out += `  ${k}: \`${`${v}`.replace(/({{([a-zA-Z0-9_]+)}})/gm, '${process.env.$2}')}\`,\n`;
  });
  return `// this file has been automatically generated\n\nexport default {\n${out}};`;
};

const toShellVarsString = (outputs: Outputs) => {
  const out = entries(outputs).map(([k, v]) => `${k}=${vartools.convertToShell(`${v}`)}\n`).join('');
  return `# this file has been automatically generated\n\n${out}`;
};

const writeVariables = (p: string, toString: () => string) => {
  if (fs.existsSync(p)) {
    const prev = fs.readFileSync(p, { encoding: 'utf8' });
    if (prev == toString())
      return;
  }

  if (fs.existsSync(p))
    fs.unlinkSync(p);

  console.log(`${colors.blue('vars-write:')} ${p}`);
  fs.writeFileSync(p, toString());

  useClean([p]);
}

export const useVarsWriter = (mode: 'ts' | 'shell', rootDir: string, vars: { [_: string]: string }) => {
  const { configurationDir } = useGlobals();
  const dir = rootDir.startsWith('/') ? rootDir : `${configurationDir}/${rootDir}`;
  if (dir) {
    if (mode == 'ts') {
      if (!fs.existsSync(`${dir}/src`))
        fs.mkdirSync(`${dir}/src`);
      writeVariables(`${dir}/src/_vars.ts`, () => toTsVarsString(vars));
      useGitIgnore(dir, ['_vars.ts']);
    } else if (mode == 'shell') {
      console.log(`${colors.blue('write-vars:')} ${colors.gray(`${dir}/_vars.env`)}`);
      writeVariables(`${dir}/_vars.env`, () => toShellVarsString(vars));
      useGitIgnore(dir, ['_vars.env']);
    } else {
      throw new Error(`unsupported variable writer mode '${mode}`);
    }
  }
}