import * as AWS from 'aws-sdk';
import * as fs from 'fs';
import { spawn } from 'child_process';
import * as RT from 'runtypes';
import * as crypto from 'crypto';
import * as mm from 'micromatch';
import * as path from 'path';
import * as colors from 'colors/safe';

const verifyRegEx = (value: unknown, errMessage: string) => {
  if (Object.prototype.toString.call(value) == '[object RegExp]')
    return true;
  error(colors.red(errMessage));
  throw new Error(errMessage);
}
const CFRecord = RT.Record({
  type: RT.Literal('cloudformation'),
  name: RT.String.withConstraint(s => verifyKey(s)),
  cfpath: RT.String,
}).And(RT.Partial({
  inputs: RT.Dictionary(RT.Union(RT.String, RT.Number)),
  outputs: RT.Array(RT.Union(RT.Record({ name: RT.String, localValue: RT.String }), RT.String)),
}));

const ShellRecord = RT.Record({
  type: RT.Literal('shell'),
  name: RT.String.withConstraint(s => verifyKey(s)),
  command: RT.String,
  args: RT.Array(RT.String),
}).And(RT.Partial({
  cwd: RT.String,
  dependsOn: RT.Array(RT.String),
  env: RT.Dictionary(RT.String),
  outputMatchers: RT.Dictionary(RT.Unknown.withConstraint(s => verifyRegEx(s, 'outputMatchers must only contain regular expressions'))),
  outputs: RT.Array(RT.Record({ name: RT.String, value: RT.String }))
}));

const Record = RT.Union(CFRecord, ShellRecord);

export type ConfigRecord = RT.Static<typeof Record>;

export type Configuration = {
  region: string,
  stage: string,
  modules: (((outputs: (k: string) => string) => ConfigRecord) | ConfigRecord)[],
};

const error = (msg: string) => console.error(colors.red(`[CONF] error: ${msg}`));
const log = (msg: string) => process.stdout.write(`[CONF] ${msg}`);

const forEachFile = (dir: string, opts: { glob: string, recurse: boolean }, continueFn: (name: string) => boolean) => {
  const d = path.resolve(dir);
  const nodes = fs.readdirSync(d);
  let done = false;
  for (let n of nodes) {
    const p = d.endsWith('/') ? `${d}${n}` : `${d}/${n}`;
    if (fs.statSync(p).isDirectory()) {
      if (opts.recurse) {
        done = forEachFile(p, opts, continueFn);
        if (done)
          break;
      }
    } else {
      if (mm.isMatch(n, opts.glob)) {
        if (!continueFn(p)) {
          done = true;
          break;
        }
      }
    }
  }
  return done;
}

const getHash = (data: { [_: string]: string | number | boolean }) => {
  const shasum = crypto.createHash('sha1');
  const sorted = Object.entries(data).sort((a, b) => a[0].localeCompare(b[0]));
  shasum.update(JSON.stringify(sorted));
  return shasum.digest('hex');
}

const lastmod = (filepath: string) => {
  if (fs.existsSync(filepath)) {
    const stats = fs.statSync(filepath);
    return new Date(stats.mtime).getTime();
  } else {
    return 0;
  }
}

const touch = (filepath: string) => {
  const time = new Date();
  try {
    fs.utimesSync(filepath, time, time);
  } catch (err) {
    fs.closeSync(fs.openSync(filepath, 'w'));
  }
}

const printOutputs = (key: string, json?: { [_: string]: string }) => {
  if (verbose && json) {
    const ents = Object.entries(json);
    if (ents.length > 0) {
      console.log('outputs:');
      ents.forEach(([k, v]) => console.log(`  ${key}_${k}=${v}`));
    }
  }
}

const _keys = {};
const verifyKey = (key: string) => {
  if (!/^[a-zA-Z0-9-_]+$/.exec(key)) {
    error(`invalid key '${key}' - only letters, digits, dashes, and underscores are allowed`);
    throw new Error(`invalid key '${key}' - only letters, digits, dashes, and underscores are allowed`);
  }
  if (_keys[key]) {
    error(`duplicate key '${key}' detected`);
    throw new Error(`duplicate key '${key}' detected`);
  }
  _keys[key] = true;
  return true;
}

const cleanJson = (data: unknown) => {
  const jprev: { [_: string]: string } = {};
  Object.entries(data).forEach(([k, v]) => {
    if (typeof v == 'string')
      jprev[k] = v;
  });
  return jprev;
}

const isAnyNewerThanCache = (key: string, dependsOn: string[]) => {
  const t1 = lastmod(`${__dirname}/.cache/${key}`);
  let ret = false;
  for (let fullglob of dependsOn) {
    const split = fullglob.split('/');
    if (split.length == 0) continue;
    const recurse = split.length > 1 && (split[split.length - 2] == '**');
    const glob = split[split.length - 1];
    if (split.length > 0) split.pop();
    if (split.length > 0 && split[split.length - 1] == '**') split.pop();
    const path = split.join('/');
    forEachFile(path.startsWith('/') ? path : `${__dirname}/../../../${path}`, { glob, recurse }, n => {
      const t2 = lastmod(n);
      if (t2 <= t1)
        return true;
      ret = true;
      return false;
    });
    if (ret)
      break;
  }
  return ret;
}

const isAnyNewerThanCache2 = (key: string, dependsOn: string[]) => {
  const t1 = lastmod(`${__dirname}/.cache/${key}`);
  for (let f of dependsOn) {
    const t2 = lastmod(`${__dirname}/../../../${f}`)
    if (t2 > t1)
      return true;
  }
  return false;
}

const cacheExists = (key: string) =>
  fs.existsSync(`${__dirname}/.cache/${key}`);

const writeCache = (key: string, data: { [k: string]: string }) => {
  if (!fs.existsSync(`${__dirname}/.cache`))
    fs.mkdirSync(`${__dirname}/.cache`);
  const out = JSON.stringify(data, null, 2);
  fs.writeFileSync(`${__dirname}/.cache/${key}`, out);
};

const readCache = (key: string): { [_: string]: string } | undefined => {
  if (fs.existsSync(`${__dirname}/.cache/${key}`)) {
    const data = fs.readFileSync(`${__dirname}/.cache/${key}`, { encoding: 'utf8' });
    try {
      const json = JSON.parse(data.toString());
      return cleanJson(json);
    } catch { }
  }
  return undefined;
}

const isInputDirty = (key: string, inputs: { [_: string]: string | number | boolean }) => {
  if (!fs.existsSync(`${__dirname}/.inputs/${key}`)) return true;
  const h1 = getHash(inputs);
  const h2 = fs.readFileSync(`${__dirname}/.inputs/${key}`, { encoding: 'utf8' });
  return h1 != h2;
}

const writeInput = (key: string, inputs: { [_: string]: string | number | boolean }) => {
  if (!fs.existsSync(`${__dirname}/.inputs`))
    fs.mkdirSync(`${__dirname}/.inputs`);
  fs.writeFileSync(`${__dirname}/.inputs/${key}`, getHash(inputs), { encoding: 'utf8' });
}

const writeEnvs = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `${k}=${v}\n`;
  });
  fs.writeFileSync(outPath, `# this file has been automatically generated\n\n${out}`);
};

const writeJs = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `// this file has been automatically generated\n\nexports.vars = {\n${out}};`);
};

const writeTs = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `// this file has been automatically generated\n\nexport const vars = {\n${out}};`);
};

const writeIgnore = (outPath: string, filesToIgnore: string[]) => {
  if (filesToIgnore.length == 0) return;
  const ents = [...filesToIgnore];
  if (!fs.existsSync(outPath)) {
    fs.writeFileSync(outPath, ents.join('\n'), { encoding: 'utf8' });
  } else {
    const data = fs.readFileSync(outPath, { encoding: 'utf8' });
    const pents = data.split('\n').filter(e => !!e);
    ents.forEach((e, i) => {
      if (pents.includes(e))
        ents[i] = undefined;
    });
    const nents = [...pents, ...ents.filter(e => !!e)];
    fs.writeFileSync(outPath, nents.join('\n'), { encoding: 'utf8' });
  }
};

const main = async (cmd: string, verbose: boolean) => {
  const configPath = 'configuration.ts';
  if (!fs.existsSync(configPath)) {
    console.log('no configuration.ts found, exiting...');
    process.exit(1);
  }

  const { default: configuration } = await import('../../../configuration');

  if (!configuration.region) {
    error('region not set');
    process.exit(1);
  }

  if (!configuration.stage) {
    error('stage not set');
    process.exit(1);
  }
  if (!['local', 'dev', 'beta', 'production'].includes(configuration.stage)) {
    error('invalid stage value - must be one of <local|dev|beta|production>');
    process.exit(1);
  }

  AWS.config.update({
    region: configuration.region,
  });

  if (configuration.modules && !Array.isArray(configuration.modules)) {
    error('invalid modules value');
    process.exit(1);
  }

  const cf = new AWS.CloudFormation({ apiVersion: '2010-05-15' });

  const stackExists = async (StackName: string) => {
    try {
      const stacks = await cf.describeStacks({
        StackName
      }).promise();
      if (stacks.Stacks.length != 1) return false;
      const s = stacks.Stacks[0];
      if (['CREATE_IN_PROGRESS', 'ROLLBACK_IN_PROGRESS', 'DELETE_IN_PROGRESS', 'DELETE_FAILED', 'UPDATE_IN_PROGRESS', 'UPDATE_COMPLETE_CLEANUP_IN_PROGRESS',
        'UPDATE_ROLLBACK_IN_PROGRESS', 'UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS', 'REVIEW_IN_PROGRESS'].includes(s.StackStatus)) {
        error(`Stack busy: ${s.StackStatus}`);
        throw new Error(`Stack busy: ${s.StackStatus}`);
      }
      if (s.StackStatus == 'ROLLBACK_COMPLETE') {
        error(`Stack '${StackName}' must be deleted manually`);
        throw new Error(`Stack '${StackName}' must be deleted manually`);
      }
      const ret = ['CREATE_COMPLETE', 'UPDATE_ROLLBACK_COMPLETE', 'UPDATE_COMPLETE', 'CREATE_COMPLETE', 'CREATE_FAILED'].includes(s.StackStatus);
      return ret;
    } catch (err) {
      if (err.code == 'ValidationError') {
        return false;
      } else {
        throw err;
      }
    }
  }

  const down = async (StackName: string) => {
    await cf.deleteStack({ StackName }).promise();
    await cf.waitFor('stackDeleteComplete', { StackName }).promise();
  }

  const up = async (StackName: string, cfPath: string, inputs: { [k: string]: string | number }, expectedOutputs: string[]) => {
    const missinginputs = [];
    for (let m in inputs)
      if (!inputs[m])
        missinginputs.push(m);
    if (missinginputs.length > 0) {
      error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`);
      throw new Error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`);
    }

    const Parameters = Object.entries(inputs).map(([k, v]) => ({ ParameterKey: k, ParameterValue: `${v}` }));
    const TemplateBody = fs.readFileSync(cfPath, 'utf8');
    const exists = await stackExists(StackName);
    try {
      if (exists) {
        process.stdout.write(`updating `);
        await cf.updateStack({
          StackName,
          TemplateBody,
          Capabilities: ['CAPABILITY_NAMED_IAM'],
          Parameters
        }).promise();

        await cf.waitFor('stackUpdateComplete', { StackName }).promise();
      } else {
        process.stdout.write(`creating...`);
        await cf.createStack({
          StackName,
          TemplateBody,
          Capabilities: ['CAPABILITY_NAMED_IAM'],
          Parameters
        }).promise();

        await cf.waitFor('stackCreateComplete', { StackName }).promise();
      }
    } catch (err) {
      if (!err.message.includes('No updates are to be performed')) {
        error(err.message);
        throw err;
      }
    }

    const desc = await cf.describeStacks({ StackName }).promise();
    if (desc.Stacks.length != 1) {
      error('failed to describe stacks');
      throw new Error('failed to describe stacks');
    }

    const outputs = desc.Stacks[0].Outputs;
    const missing = [];
    for (let eo in expectedOutputs)
      if (!outputs[eo])
        missing.push(eo);
    if (missing.length > 0) {
      error(`missing outputs (${missing.join(', ')})`);
      throw new Error(`missing outputs (${missing.join(', ')})`);
    }
    const unused = [];
    for (let o in outputs)
      if (!expectedOutputs)
        unused.push(o);
    if (unused.length > 0)
      console.log(`unused outputs (${unused.join(', ')})`);
    const ret = {};
    outputs.forEach(o => ret[o.OutputKey] = o.OutputValue);
    return ret;
  };

  const parseModule = (m, prev) => {
    const ret = typeof m == 'function' ? m(prev) : m;
    return ret &&
      ret.type && typeof ret.type == 'string' &&
      ret.key && typeof ret.key == 'string' &&
      (ret.inputs && typeof ret.inputs == 'object' || true) &&
      (ret.outputs && typeof ret.outputs == 'object' || true) && ret || undefined;
  }

  const appendPrev = (previous: { [_: string]: string }, key: string, prev: { [_: string]: string }) => {
    const p = {};
    Object.entries(prev).map(([k, v]) => p[`${key}_${k}`] = v);
    return { ...previous, ...p };
  }

  const getStackname = (name: string) => `${configuration.stage}-${name}`;

  if (cmd == 'up') {
    let previous = {};
    for (let f of configuration.modules) {
      const rec = typeof f == 'function' ? f(k => {
        const ret = previous[k];
        if (!ret) throw new Error(`invalid output variable '${k}'`);
        return ret;
      }) : f;

      const ret = await Record.match(

        async cloudformation => {
          const { name, inputs, outputs, cfpath: cfpath2 } = cloudformation;
          const key = name.replace(/-/g, '_').toUpperCase();
          log(`${name} `);

          if (configuration.stage == 'local') {
            const tsdir = `${__dirname}/../../../node_modules/@inf/${name}`;
            if (!fs.existsSync(tsdir))
              fs.mkdirSync(tsdir);
            if (!fs.existsSync(`${tsdir}/src`))
              fs.mkdirSync(`${tsdir}/src`);

            const prev = {};
            outputs && outputs.forEach(o => typeof o == 'string' ? prev[o] = 'LOCAL_UNUSED' : prev[o.name] = o.localValue);
            previous = appendPrev(previous, key, prev);
            writeTs(`${tsdir}/src/vars.ts`, key, prev);
            writeJs(`${tsdir}/src/vars.js`, key, prev);

            console.log('');
            return true;
          } else {
            const inputDirty = isInputDirty(key, inputs);
            const cfpath = `${__dirname}/../../../node_modules/@inf/${name}/${cfpath2}`;
            if (!fs.existsSync(cfpath)) {
              error(`invalid cf module ${name} - ${cfpath} not found`);
              throw new Error(`invalid cf module ${name} - ${cfpath} not found`);
            }

            const ot = lastmod(`${__dirname}/.cache/${key}`);
            const cfDirty = lastmod(cfpath) > ot;
            if (!cfDirty && !inputDirty) {
              console.log('');
              const prev = readCache(key);
              if (prev) {
                previous = appendPrev(previous, key, prev);
                return true;
              }
            }

            const outs = outputs.map(o => typeof o == 'string' ? o : o.name);
            const prev = await up(getStackname(name), cfpath, inputs, outs);
            writeCache(key, prev);
            writeInput(key, inputs);
            previous = appendPrev(previous, key, prev);

            const tsdir = `${__dirname}/../../../node_modules/@inf/${name}`;
            if (!fs.existsSync(tsdir))
              fs.mkdirSync(tsdir);
            if (!fs.existsSync(`${tsdir}/src`))
              fs.mkdirSync(`${tsdir}/src`);
            writeTs(`${tsdir}/src/vars.ts`, key, prev);
            writeJs(`${tsdir}/src/vars.js`, key, prev);
            writeIgnore(`${tsdir}/.gitignore`, ['src/vars.ts', 'src/vars.js', 'lib']);

            console.log('(updated)');
            return true;
          }
        },

        shell => new Promise((resolve, reject) => {
          const { name, command, args, env, dependsOn, outputMatchers, cwd, outputs } = shell;

          const key = name.replace(/-/g, '_').toUpperCase();
          log(`${name} `);

          let dirty = false;
          const prev = {};

          if (dependsOn) {
            if (dependsOn.length == 0) {
              if (!cacheExists(key))
                dirty = true;
            } else {
              if (isAnyNewerThanCache(key, dependsOn))
                dirty = true;
            }
          } else {
            dirty = true;
          }

          if (!dirty) {
            const prev = readCache(key);
            if (prev) {
              previous = appendPrev(previous, key, prev);
              console.log('');
              resolve(true);
              return;
            }
          }

          const tsdir = `${__dirname}/../../../node_modules/@inf/${name}`;
          if (fs.existsSync(`${tsdir}/vars.env`)) fs.unlinkSync(`${tsdir}/vars.env`);
          if (fs.existsSync(`${tsdir}/vars.ts`)) fs.unlinkSync(`${tsdir}/vars.ts`);
          if (fs.existsSync(`${tsdir}/vars.js`)) fs.unlinkSync(`${tsdir}/vars.js`);

          const cwdEnabled = !command.startsWith('.') && !command.startsWith('/') && cwd;
          const cwd2 = cwdEnabled ? cwd : path.dirname(command);
          const cmd = cwdEnabled ? command : path.basename(command);
          const proc = spawn(cmd, args, { env: { ...process.env, ...env }, cwd: cwd2 });
          let buffer = '';
          proc.stdout.on('data', data => {
            const buf = data.toString();
            if (verbose) {
              if (!buffer.length) console.log('');
              process.stdout.write(colors.gray(buf));
            }
            buffer += buf;
          });
          proc.stderr.on('data', data => process.stderr.write(data.toString()));
          proc.on('close', code => {
            if (code == 0) {

              if (verbose && buffer.length > 0)
                console.log('');

              let json = undefined;
              try { json = cleanJson(JSON.parse(buffer.trim())); } catch (ex) { }
              if (json) {
                buffer = '';
                printOutputs(key, json);
                writeCache(key, json);
                previous = appendPrev(previous, key, json);
              } else {
                let json = undefined;
                if (outputMatchers) {
                  const matchers = Object.entries(outputMatchers);
                  matchers.forEach(([k, m]) => {
                    const match = (m as RegExp).exec(buffer);
                    if (match && match.length > 1) {
                      if (!json) json = {};
                      json[k] = match[1];
                    }
                  });
                }

                if (outputs) {
                  let json2 = undefined;
                  outputs.forEach(o => {
                    if (!json2) json2 = {};
                    json2[o.name] = o.value;
                  });
                  if (!fs.existsSync(tsdir))
                    fs.mkdirSync(tsdir);
                  writeEnvs(`${tsdir}/vars.env`, key, json2);
                  writeTs(`${tsdir}/vars.ts`, key, json2);
                  writeJs(`${tsdir}/vars.js`, key, json2);
                  writeIgnore(`${tsdir}/.gitignore`, ['vars.env', 'vars.ts', 'vars.js', 'lib']);
                }

                buffer = '';
                if (json) {
                  printOutputs(key, json);
                  writeCache(key, json);
                  previous = appendPrev(previous, key, json);
                } else {
                  printOutputs(key, json);
                  writeCache(key, prev);
                  previous = appendPrev(previous, key, prev);
                }
              }
            }
            if (code != 0) {
              reject(new Error('shell failed'));
            } else {
              console.log('(updated)');
              resolve(true);
            }
          });
        }),
      )(rec);

      if (ret == undefined) {
        error(colors.red(`record match failed: ${JSON.stringify(rec, null, 2)}`));
        process.exit(1);
      }

      //console.logLn('PREV', previous);
    }
  } else if (cmd == 'down') {
    let previous = {};
    const ms = configuration.modules.map(f => {
      const n = typeof f == 'function' ? f(() => '') : f;
      const ret = parseModule(f, previous);
      const p = {};
      //if (n.outputs && Array.isArray(n.outputs))
      //n.outputs.forEach(o => p[o] = true);
      previous = {};
      return ret;
    });
    for (let i = ms.length; i > 0; --i) {
      const m = ms[i - 1];
      if (m) await down(getStackname(m.name));
    }
  } else {
    error(`unknown configure command '${cmd}'`);
    throw new Error(`unknown configure command '${cmd}'`);
  }
};

let verbose = false;

const [_1, _2, ...rest] = process.argv;
const cmdargs = rest.filter(r => {
  if (r.startsWith('-')) {
    if (r == '-v' || r == '--verbose')
      verbose = true;
    return false;
  } else {
    return true;
  }
});

if (cmdargs.length == 0) {
  console.log(`usage: configure <up|down>`);
  process.exit(1);
}

const cmd = cmdargs[0];
main(cmd, verbose).catch(err => {
  error(err && err.message || err);
  process.exit(1);
});
