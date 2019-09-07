// @ts-check

const fs = require('fs');
const colors = require('colors/safe');

const repoDir = `${__dirname}/../../..`;
let g_verbose = false;

const genTypes = (outfile, env) => {
  const data = `// this file has been automatically generated

type Vars = {
${Object.keys(env).map(k => `  ${k}: string`).join(',\n')}
};

export const vars: Vars;
`;
  fs.writeFileSync(outfile, data, { encoding: 'utf8' });
}

const lastmod = (filepath) => {
  if (fs.existsSync(filepath)) {
    const stats = fs.statSync(filepath);
    return new Date(stats.mtime).getTime();
  } else {
    return 0;
  }
}

const makeSafe = env => {
  let ret = {};
  Object.entries(env).map(([k, v]) => {
    ret = {
      ...ret, get [k]() {
        if (env.STAGE == 'local' && ['AWS_ACCOUNT_ID', 'AWS_ACCESS_KEY_ID', 'AWS_SECRET_ACCESS_KEY'].includes(k)) {
          return 'LOCAL_UNDEFINED';
        } else {
          const v = env[k];
          if (!v) throw new Error(`undefined envvar detected ${k}`);
          return v;
        }
      }
    };
  });
  return ret;
}

const parseJson = (key, jsonPath) => {
  if (!fs.existsSync(jsonPath))
    return {};

  const data = fs.readFileSync(jsonPath, { encoding: 'utf-8' });
  try {
    const json = JSON.parse(data);
    let ret = {};
    Object.entries(json).forEach(([k, v]) => { ret = { ...ret, [`${key}_${k}`]: v }; });
    return ret;
  } catch {
    return {};
  }
}

const parseEnv = envsPath => {
  const path = `${repoDir}/${envsPath}`;

  if (g_verbose)
    console.log(`including vars from: ${path}`);

  const ret = {};
  if (fs.existsSync(path)) {
    const envsData = fs.readFileSync(path, { encoding: 'utf-8' });
    envsData.split('\n').forEach(line => {
      const l = line.trim();
      if (!l || l.startsWith('#')) return;
      const split = l.split('=');
      if (split.length < 2) return;
      const key = split[0];
      const value = split[1];
      ret[key] = value;
    });
  }
  return ret;
}

const fromEntries = (ents) => ents.reduce((a, [k, v]) => { a[k] = v; return a }, {});

const maskEnv = (envs, examples = undefined) => {
  return fromEntries(Object.entries(envs).map(([key, value]) => {
    if (examples && examples[key])
      examples[key] = { done: true, secret: examples[key] && examples[key].secret || false };
    const issecret = examples[key] && examples[key].secret;
    return issecret ? [key, `{{${key}}}`] : [key, value];
  }));
}

const parseExamples = examplespath => {
  if (!examplespath) return undefined;
  const examplefields = {};
  const examplesPath = `${repoDir}/${examplespath}`;
  if (examplespath && fs.existsSync(examplesPath)) {
    const examp = fs.readFileSync(examplesPath, { encoding: 'utf-8' });
    examp.split('\n').forEach(line => {
      const l = line.trim();
      if (l.length < 2) return;
      if (!l.endsWith('=') && !l.endsWith('=?')) {
        console.warn(`invalid syntax found in ${examplesPath}`);
        process.exit(1);
      }
      if (l.endsWith('='))
        examplefields[l.substring(0, l.length - 1)] = { done: false, secret: false };
      else
        examplefields[l.substring(0, l.length - 2)] = { done: false, secret: true };
    });
  }
  return examplefields;
}

const verifyEnv = (envs, examples) => {
  Object.entries(envs).forEach(([key]) => {
    if (!examples || (examples[key]))
      examples[key] = { done: true, secret: examples[key] && examples[key].secret || false };
  });

  if (examples) {
    const missingKeys = Object.entries(examples).filter(([k, v]) => v && !v.done).map(([k]) => k);
    if (missingKeys.length > 0) {
      console.warn(`the environment is missing the following keys: ${colors.red(missingKeys.join(', '))}`);
      process.exit(1);
    }
  }
}

const examples = parseExamples('.env.example');

const additionalEnvPaths = fs.existsSync(`${__dirname}/.cache`) ? fs.readdirSync(`${__dirname}/.cache`).map(f =>
  ({ key: f, path: `${__dirname}/.cache/${f}` })) : [];

const additionalEnv = additionalEnvPaths.reduce((a, p) => ({ ...a, ...parseJson(p.key, p.path) }), {});

const env = makeSafe(parseEnv('.env'));

if (lastmod('.env') > lastmod(`${__dirname}/vars.d.ts`))
  genTypes(`${__dirname}/vars.d.ts`, env);

const vars = { ...additionalEnv, ...env, ...process.env };
verifyEnv(vars, examples);

exports._vars = vars;
exports.vars = maskEnv(vars, examples);