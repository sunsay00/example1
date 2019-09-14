import * as fs from 'fs';
import * as colors from 'colors/safe';
import * as path from 'path';
import { spawn } from 'child_process';
import { fromEntries, entries } from '@inf/common';
import { vartools, substituteVariables } from './vartools';
import { StdioOptions, ScriptFlag, ScriptRule, Scripts } from '.';
import { substitute } from '@inf/common';

// @ts-ignore
import * as V from '../../vars';

export const main = async (argv: string[]) => {
  const [_1, cmd, ...args] = argv;

  const printcommands = (msg?: string) => {
    if (msg) console.log(colors.red(`${msg}\n`));
    console.log(`usage: ${path.basename(cmd)} <command> <subcommand> [...<args>]`);
    console.log('');
    if (fs.existsSync(`${__dirname}/../../.scripts`)) {
      console.log('available commands:');
      console.log('    COMMANDS');
      fs.readdirSync(`${__dirname}/../../.scripts`).sort((a, b) => a.localeCompare(b)).forEach(p => {
        const path = p.replace(/(.*)\.json$/, '$1');
        console.log(`    ${colors.grey('-')} ${colors.bold(path)}`);
      });
    }
  }

  const printrules = (command: string, rules: { [_: string]: ScriptRule }, msg?: string) => {
    if (msg) console.log(colors.red(`${msg}\n`));
    console.log(`usage: ${path.basename(cmd)} ${command} <subcommand> [...<args>]`);
    console.log('');
    console.log(`available rules for '${command}':`);
    console.log('    RULES                               DESCRIPTION');
    entries(rules).sort((a, b) => a[0].localeCompare(b[0])).forEach(([k, cmd]) => {
      if (cmd.desc) {
        console.log(`    ${colors.grey('-')} ${colors.bold(k.substr(0, 30).padEnd(30, ' '))}    ${colors.grey(cmd.desc || '')}`);
      }
    });
  }

  if (process.argv.length <= 2) {
    printcommands();
    process.exit(0);
  }

  if (args.length == 0) {
    printcommands();
    process.exit(0);
  }

  const [command, ...subargs] = args;
  if (!fs.existsSync(`${__dirname}/../../.scripts/${command}.json`)) {
    printcommands(`command '${command}' not found`);
    process.exit(0);
  }

  let json = undefined;
  try {
    json = JSON.parse(fs.readFileSync(`${__dirname}/../../.scripts/${command}.json`, { encoding: 'utf8' }));
  } catch (err) { }

  if (!json) {
    console.error('invalid script', json);
    process.exit(1);
  }

  const { env, cwd, rules } = json as Scripts;

  if (subargs.length == 0) {
    printrules(command, rules);
    process.exit(0);
  }

  const [rulename, ...params] = subargs;

  const rule = rules[rulename];
  if (!rule) {
    printrules(command, rules, `rule '${rulename}' not found`);
    process.exit(0);
  }

  const prepareVariables = (scriptenv: { [_: string]: string }) => {
    const missingVars = [] as string[];
    const ret = fromEntries(entries(scriptenv).map(([k, s]) => {
      // @ts-ignore
      const str = substituteVariables({ ...V._vars, ...process.env })(`${s}`);
      if (str == undefined) missingVars.push(k);
      return [k, str];
    }));
    if (missingVars.length > 0)
      throw new Error(`envvars [${missingVars.join(', ')}] must be defined`);
    return ret;
  }

  const prepareParameterFlags = (params: string[], flags: { [_: string]: ScriptFlag }) => {
    const pset = {} as { [_: string]: string };
    let f = '';
    params.forEach(p => {
      if (p.startsWith('-'))
        f = p;
      else if (f) {
        pset[f] = p
        f = '';
      }
    })
    if (f) throw new Error(`invalid flag ${f}`);
    const reqcount = entries(flags).reduce((a, [k, f]) => a + (f.default ? 0 : (pset[`--${k}`] == undefined && pset[`-${f.shortcut}`] == undefined) ? 1 : 0), 0);
    if (reqcount > 0 || params.length == 1 && params[0] == 'help') {
      console.log('    FLAGS                             DEFAULT          DESCRIPTION');
      entries(flags).sort((a, b) => a[0].localeCompare(b[0])).forEach(([k, flag]) => {
        console.log(`    ${colors.bold(`${`--${k} ${flag.shortcut ? ` -${flag.shortcut}` : ''}`}`.substr(0, 33).padEnd(33, ' '))} ${flag.default ? flag.default.substr(0, 16).padEnd(16, ' ') : '                '} ${colors.grey(flag.desc || '')}`);
      });
      process.exit(0);
    }
    return fromEntries(entries(flags).map(([k, v]) => [`flags:${k}`, pset[`--${k}`] || pset[`-${v.shortcut}`] || v.default]));
  }

  const invokeRule = (
    stdio: StdioOptions | undefined,
    cwd: string | undefined,
    scriptenv: { [_: string]: string },
    params: string[],
    { command, args }: { command: string, args?: string[] },
    flags?: { [_: string]: ScriptFlag }
  ) => new Promise((resolve, reject) => {
    const fullargs = flags ? (args || []).map(substitute(/({{([a-zA-Z0-9_:]+)}})/gm, prepareParameterFlags(params, flags))) : (args || []);
    console.log(colors.grey(`${colors.blue('script:')} ${command} ${fullargs.join(' ')} # (CWD: ${cwd})`));
    const env = {
      // @ts-ignore
      ...V._vars,
      ...process.env,
      ...prepareVariables(scriptenv),
      FORCE_COLOR: 'true'
    };
    const convertedargs = fullargs.map(vartools.convertToShell);

    const proc = spawn(command, convertedargs, { shell: true, cwd, env, stdio });
    proc.stdout && proc.stdout.on('data', data => process.stdout.write(colors.grey(data.toString())));
    proc.stderr && proc.stderr.on('data', data => process.stderr.write(colors.red(data.toString())));
    proc.on('close', code => {
      console.log('');
      if (code == 0) resolve();
      else reject(new Error(`failed to invoke make rule '${rulename}'`));
    });
    proc.stdin && proc.stdin.end();
  })

  const run = async (
    stdio: StdioOptions | undefined,
    cwd: string | undefined,
    env: { [_: string]: string },
    params: string[],
    commands?: { command: string, arg?: string[] }[],
    flags?: { [_: string]: ScriptFlag },
  ) => {
    if (!commands || commands.length == 0) return;
    const [fst, ...rst] = commands;
    if (!fst.command) return;
    await invokeRule(stdio, cwd, env || {}, params, fst, flags);
    await run(stdio, cwd, env, params, rst, flags);
  }

  await run(rule.stdio, rule.cwd || cwd, { ...(env || {}), ...(rule.env || {}) }, params, rule.commands, rule.flags);
}