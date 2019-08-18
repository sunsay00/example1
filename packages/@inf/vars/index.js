// @ts-check

const fs = require('fs');

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

const makeSafe = env => {
  let ret = {};
  Object.entries(env).map(([k, v]) => {
    ret = {
      ...ret, get [k]() {
        const v = env[k];
        if (!v) throw new Error(`Undefined envvar detected ${k}`);
        return v;
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

const parseEnv = (envsPath, examples = undefined) => {
  const path = `${repoDir}/${envsPath}`;
  if (!fs.existsSync(path))
    return {};

  const examplefields = {};
  const examplesPath = `${repoDir}/${examples}`;
  if (examples && fs.existsSync(examplesPath)) {
    const examp = fs.readFileSync(examplesPath, { encoding: 'utf-8' });
    examp.split('\n').forEach(line => {
      const l = line.trim();
      if (l.length < 2) return;
      if (!l.endsWith('=')) {
        console.warn(`invalid syntax found in ${examplesPath}`);
        process.exit(1);
      }
      examplefields[l.substring(0, l.length - 1)] = 1;
    });
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
    const key = split[0];
    const value = split[1];
    if (!examples || (examples && examplefields[key] == 1))
      examplefields[key] = 0;
    ret[key] = value;
  });

  if (examples) {
    const missingKeys = Object.entries(examplefields).filter(([k, v]) => v != 0).map(([k]) => k);
    if (missingKeys.length > 0) {
      console.warn(`${envsPath} is missing the following keys: ${missingKeys.join(', ')}`);
      process.exit(1);
    }
  }

  return ret;
}

const additionalEnvPaths = fs.existsSync(`${__dirname}/.cache`) ? fs.readdirSync(`${__dirname}/.cache`).map(f =>
  ({ key: f, path: `${__dirname}/.cache/${f}` })) : [];

const additionalEnv = additionalEnvPaths.reduce((a, p) => ({ ...a, ...parseJson(p.key, p.path) }), {});

const env = makeSafe({
  ...parseEnv('envs'),
  ...parseEnv('.env', '.env.example')
});

genTypes(`${__dirname}/index.d.ts`, env);

exports.vars = { ...additionalEnv, ...env, ...process.env };