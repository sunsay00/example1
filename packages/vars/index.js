#!/usr/bin/env node

const path = require('path');
const fs = require('fs');
const { spawn } = require("child_process");

const repoDir = `${__dirname}/../..`;
let g_verbose = false;

const parseEnvs = (envsPath, required = false) => {
  const path = `${repoDir}/${envsPath}`;
  if (!fs.existsSync(path)) {
    if (!required) return {};
    console.error(`envs file not found - looked in ${path}`);
    process.exit(1);
  }

  if (g_verbose)
    console.log(`including vars from: ${path}`);

  const envsData = fs.readFileSync(path, { encoding: 'utf-8' });
  const ret = {};
  envsData.split('\n').forEach(line => {
    const l = line.trim();
    if (!l || l.startsWith('#')) return;
    const split = l.split('=');
    if (split.length < 2) return;
    ret[split[0]] = split[1];
  });
  return ret;
}

const argv = [];
process.argv.forEach(v => {
  if (v == '-V') {
    g_verbose = true;
    return;
  } else {
    argv.push(v);
  }
});

if (argv.length < 2) {
  console.log(`usage: ${path.basename(argv[1])} <cmd> [...<args>]`);
  process.exit(1);
}

let env = { ...process.env };
const additionalEnvPaths = fs.readdirSync(`${repoDir}/.envs`).map(f => `.envs/${f}`);
additionalEnvPaths.forEach(p => { env = { ...env, ...parseEnvs(p) }; });

env = { ...env, ...parseEnvs(`envs`, true) };
const stage = env.STAGE || 'dev';
env = { ...env, ...parseEnvs(`.secrets.${stage}`) };
env = { ...parseEnvs(`envs.${stage}`), ...env, STAGE: stage };

if (argv.length == 2) {
  Object.entries(env).forEach(([k, v]) => console.log(`${k}=${v}`));
} else {
  const [_, __, cmd, ...args] = argv;
  const proc = spawn(cmd, args, { env, stdio: [process.stdin, process.stdout, 'pipe'] });
  proc.stderr.on('data', data => process.stderr.write(data.toString()));
  proc.on('close', code => process.exit(code));
}
