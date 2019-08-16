const fs = require('fs');

const repoDir = `${__dirname}/../../..`;
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

const rootEnv = parseEnvs('envs', true);
const stage = process.env.STAGE || rootEnv.STAGE || 'dev';

let env = { ...process.env };
const additionalEnvPaths = fs.readdirSync(`${repoDir}/.envs.${stage}`).map(f => `.envs.${stage}/${f}`);
env = { ...env, ...additionalEnvPaths.reduce((a, p) => ({ ...a, ...parseEnvs(p) }), {}) };
env = { ...env, ...rootEnv };
env = { ...env, ...parseEnvs(`.secrets.${stage}`) };
env = { ...parseEnvs(`envs.${stage}`), ...env, STAGE: stage };

exports.vars = env;