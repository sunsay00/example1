import * as fs from 'fs';
import * as path from 'path';
import { spawn } from 'child_process';

type Rule = { name: string, desc?: string };

const readRules = (makefilepath: string): Rule[] => {
  const data = fs.readFileSync(makefilepath, { encoding: 'utf8' });
  //const re = /^(?:# DESC: (.+)?[\w\W])?([a-zA-Z0-9][a-zA-Z0-9\.]*):/gm;
  const re = /^(?:# DESC: (.+)?[\w\W])([a-zA-Z0-9][a-zA-Z0-9\.]*):/gm;
  let m = re.exec(data);
  const rules = [];
  while (m) {
    if (m[1] && m[2]) {
      rules.push({ name: m[2], desc: m[1] });
    } else if (m[2]) {
      rules.push({ name: m[2] });
    }
    m = re.exec(data);
  }
  return rules;
}

const invokeRule = (makefilepath: string, rule: string, args: string[]) => new Promise<void>((resolve, reject) => {
  const proc = spawn('make', [rule], {
    shell: true,
    cwd: path.dirname(makefilepath),
    env: {
      ...process.env,
      SLS_SS_ARGS: `"${args.map(s => s.replace(/'/g, '\'')).join(' ')}"`,
      FORCE_COLOR: 'true'
    }
  });
  proc.stdout.on('data', data => process.stdout.write(data.toString()));
  proc.stderr.on('data', data => process.stderr.write(data.toString()));
  proc.on('close', code => {
    if (code == 0) resolve();
    else reject(new Error(`failed to invoke make rule '${rule}'`));
  });
  proc.stdin.end();
})

const printRules = (rules: Rule[]) => {
  console.log(`usage: ${path.basename(process.argv[1])} <command> ...`);
  console.log('    COMMAND                             DESCRIPTION');
  rules.forEach(cmd => console.log(`    - ${cmd.name.substr(0, 30).padEnd(30, ' ')}    ${cmd.desc || ''}`));
}

export const main = async (makefilepath: string, argv: string[]) => {
  if (!fs.existsSync(makefilepath)) {
    console.error('invalid makefile');
    process.exit(1);
  }
  const cmds = maketools.readRules(makefilepath);
  if (argv.length < 3) {
    maketools.printRules(cmds);
  } else {
    const [_1, _2, ...args] = argv;
    if (args.length > 0) {
      const [cmd, ...subargs] = args;
      if (cmds.find(c => c.name == cmd)) {
        await maketools.invokeRule(makefilepath, cmd, subargs);
      } else {
        maketools.printRules(cmds);
      }
    } else {
      maketools.printRules(cmds);
    }
  }
};

export const maketools = {
  main,
  readRules,
  invokeRule,
  printRules
};