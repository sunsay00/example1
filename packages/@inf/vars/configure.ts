import * as AWS from 'aws-sdk';
import * as fs from 'fs';
import { spawn, execSync } from 'child_process';
import * as RT from 'runtypes';
import * as crypto from 'crypto';
import * as mm from 'micromatch';
import * as path from 'path';
import * as colors from 'colors/safe';
import { entries, Diff } from '@inf/common';

const verifyRegEx = (value: unknown, errMessage: string) => {
  if (Object.prototype.toString.call(value) == '[object RegExp]')
    return true;
  error(colors.red(errMessage));
  throw new Error(errMessage);
}
const CloudFormationRecord = RT.Record({
  type: RT.Literal('cloudformation'),
  name: RT.String.withConstraint(s => verifyKey(s)),
  cfpath: RT.String,
}).And(RT.Partial({
  inputs: RT.Dictionary(RT.Union(RT.String, RT.Number, RT.Array(RT.String))),
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
  outputs: RT.Dictionary(
    RT.Union(
      RT.String,
      RT.Record({
        outputMatcher: RT.Unknown.withConstraint(s => verifyRegEx(s, 'outputMatchers must only contain regular expressions'))
      })))
}));

const Record = RT.Union(CloudFormationRecord, ShellRecord);

export type CFEvent = 'CREATE' | 'CLEAN';

type CFRecord = RT.Static<typeof Record>;
export type CFParams = {
  configurationDir: string
};
export type ConfigRecord = { record: (event: CFEvent, params: CFParams) => CFRecord };
export const createConfig = (rc: ConfigRecord | ((params: CFParams) => ConfigRecord)) => ({
  record: (event: CFEvent, params: CFParams) => typeof rc == 'function' ? rc(params).record(event, params) : rc.record(event, params)
});
export const createRecord = (rc: CFRecord | ((event: CFEvent, params: CFParams) => CFRecord)) => ({
  record: typeof rc == 'function' ? rc : () => rc
});
export const createConfigRecord = (rc: CFRecord | ((event: CFEvent, params: CFParams) => CFRecord)) => createConfig(createRecord(rc));

type ConfigRecordFn = (outputs: (k: string) => string) => ConfigRecord;
type ModuleRecord = ConfigRecordFn | ConfigRecord;

export type ShellOutput = Diff<RT.Static<typeof ShellRecord>['outputs'], undefined>;
export type CFOutput = Diff<RT.Static<typeof CloudFormationRecord>['outputs'], undefined>;

export type Configuration = {
  region: string,
  stage: string,
  modules: (ModuleRecord | ModuleRecord[])[],
};

const error = (msg: string, id?: number) => console.error(colors.red(`[CONF] error: ${msg}${id ? ` (${id})` : ''}`));
const log = (msg: string) => process.stdout.write(`[CONF] ${msg}`);

const forEachFile = (dir: string, opts: { glob: string, recurse: boolean }, continueFn: (name: string) => boolean): boolean => {
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

const getHash = (data: { [_: string]: string | number | boolean | string[] }) => {
  const shasum = crypto.createHash('sha1');
  const sorted = entries(data).sort((a, b) => a[0].localeCompare(b[0]));
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

const _keys: { [_: string]: boolean } = {};
const verifyKey = (key: string) => {
  if (!/^[a-zA-Z0-9-_]+$/.exec(key)) {
    error(`invalid key '${key}' - only letters, digits, dashes, and underscores are allowed`, 1);
    throw new Error(`invalid key '${key}' - only letters, digits, dashes, and underscores are allowed`);
  }
  if (_keys[key]) {
    error(`duplicate key '${key}' detected`, 2);
    throw new Error(`duplicate key '${key}' detected`);
  }
  _keys[key] = true;
  return true;
}

const cleanJson = (data: {}) => {
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

const isInputDirty = (key: string, inputs?: { [_: string]: string | number | boolean | string[] }) => {
  if (!inputs || !fs.existsSync(`${__dirname}/.inputs/${key}`)) return true;
  const h1 = getHash(inputs);
  const h2 = fs.readFileSync(`${__dirname}/.inputs/${key}`, { encoding: 'utf8' });
  return h1 != h2;
}

const writeInput = (key: string, inputs?: { [_: string]: string | number | boolean | string[] }) => {
  if (!fs.existsSync(`${__dirname}/.inputs`))
    fs.mkdirSync(`${__dirname}/.inputs`);
  if (!inputs) {
    if (fs.existsSync(`${__dirname}/.inputs/${key}`))
      fs.unlinkSync(`${__dirname}/.inputs/${key}`);
  } else {
    fs.writeFileSync(`${__dirname}/.inputs/${key}`, getHash(inputs), { encoding: 'utf8' });
  }
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
  const ents = [...filesToIgnore].sort((a, b) => a.localeCompare(b));
  if (!fs.existsSync(outPath)) {
    fs.writeFileSync(outPath, ents.join('\n'), { encoding: 'utf8' });
  } else {
    const data = fs.readFileSync(outPath, { encoding: 'utf8' });
    const pents = data.split('\n').filter(e => !!e);
    ents.forEach((e, i) => {
      if (pents.includes(e))
        ents[i] = '';
    });
    const nents = [...pents, ...ents.filter(e => !!e)].sort((a, b) => a.localeCompare(b));
    fs.writeFileSync(outPath, nents.join('\n'), { encoding: 'utf8' });
  }
};

const main = async (cmd: string, verbose: boolean) => {
  const configPath = 'configuration.ts';
  const configurationDir = path.dirname(path.resolve(configPath));
  if (!fs.existsSync(configPath)) {
    console.log('no configuration.ts found, exiting...');
    process.exit(1);
  }

  const { default: configuration } = await import('../../../configuration');

  if (!configuration.region) {
    error('region not set', 3);
    process.exit(1);
  }

  if (!configuration.stage) {
    error('stage not set', 4);
    process.exit(1);
  }
  if (!['local', 'dev', 'beta', 'production'].includes(configuration.stage)) {
    error('invalid stage value - must be one of <local|dev|beta|production>', 5);
    process.exit(1);
  }

  AWS.config.update({
    region: configuration.region,
  });

  if (configuration.modules && !Array.isArray(configuration.modules)) {
    error('invalid modules value', 6);
    process.exit(1);
  }

  const cf = new AWS.CloudFormation({ apiVersion: '2010-05-15' });

  const stackExists = async (StackName: string) => {
    try {
      const stacks = await cf.describeStacks({
        StackName
      }).promise();
      if (!stacks.Stacks || stacks.Stacks.length != 1) return false;
      const s = stacks.Stacks[0];
      if (['CREATE_IN_PROGRESS', 'ROLLBACK_IN_PROGRESS', 'DELETE_IN_PROGRESS', 'DELETE_FAILED', 'UPDATE_IN_PROGRESS', 'UPDATE_COMPLETE_CLEANUP_IN_PROGRESS',
        'UPDATE_ROLLBACK_IN_PROGRESS', 'UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS', 'REVIEW_IN_PROGRESS'].includes(s.StackStatus)) {
        error(`Stack busy: ${s.StackStatus}`, 7);
        throw new Error(`Stack busy: ${s.StackStatus}`);
      }
      if (s.StackStatus == 'ROLLBACK_COMPLETE') {
        error(`Stack '${StackName}' must be deleted manually`, 8);
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

  const up = async (StackName: string, cfPath: string, inputs: { [k: string]: string | number | string[] } | undefined, expectedOutputs: string[]) => {
    const missinginputs = [];
    for (let m in inputs)
      if (!inputs[m])
        missinginputs.push(m);
    if (missinginputs.length > 0) {
      error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`, 9);
      throw new Error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`);
    }

    const Parameters = inputs && entries(inputs).map<AWS.CloudFormation.Parameter>(([k, v]) => ({
      ParameterKey: k,
      ParameterValue: Array.isArray(v) ? `${v.map(i => `${i}`).join(',')}` : `${v}`
    }));
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
        error(err.message, 10);
        throw err;
      }
    }

    const desc = await cf.describeStacks({ StackName }).promise();
    if (!desc.Stacks || desc.Stacks.length != 1) {
      error('failed to describe stacks', 11);
      throw new Error('failed to describe stacks');
    }

    const outputs = desc.Stacks[0].Outputs || [];
    const missing = [];
    for (let eo in expectedOutputs)
      if (!outputs[eo])
        missing.push(eo);
    if (missing.length > 0) {
      error(`missing outputs (${missing.join(', ')})`, 12);
      throw new Error(`missing outputs (${missing.join(', ')})`);
    }
    const unused = [];
    for (let o in outputs)
      if (!expectedOutputs)
        unused.push(o);
    if (unused.length > 0)
      console.log(`unused outputs (${unused.join(', ')})`);
    const ret: { [_: string]: string } = {};
    outputs.forEach(o => { if (o.OutputKey && o.OutputValue) ret[o.OutputKey] = o.OutputValue; });
    return ret;
  };

  const appendPrev = (previous: { [_: string]: string }, key: string, prev: { [_: string]: string }) => {
    const p: { [_: string]: string } = {};
    Object.entries(prev).map(([k, v]) => p[`${key}_${k}`] = v);
    return { ...previous, ...p };
  }

  const getStackname = (name: string) => `${configuration.stage}-${name}`;

  let previous: { [_: string]: string } = {};
  const modules: ModuleRecord[] = [];
  for (let m of configuration.modules) {
    if (Array.isArray(m)) {
      m.forEach(i => modules.push(i));
    } else {
      modules.push(m);
    }
  }

  if (cmd == 'clean') {
    const tmpinputsdir = `${__dirname}/.inputs`;
    const tmpcachedir = `${__dirname}/.cache`;
    if (fs.existsSync(tmpinputsdir))
      execSync(`rm -rf ${tmpinputsdir}`);
    if (fs.existsSync(tmpcachedir))
      execSync(`rm -rf ${tmpcachedir}`);

    for (let f of modules) {
      const rec = typeof f == 'function' ? f(k => previous[k]) : f;
      rec.record('CLEAN', { configurationDir });
    }
  } else if (cmd == 'up') {
    for (let f of modules) {
      const rec = typeof f == 'function' ? f(k => {
        const ret = previous[k];
        if (!ret) {
          error(`invalid output variable '${k}'`, 13);
          throw new Error(`invalid output variable '${k}'`);
        }
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

            const prev: { [_: string]: string } = {};
            outputs && outputs.forEach(o => typeof o == 'string' ? prev[o] = 'LOCAL_UNUSED' : prev[o.name] = o.localValue);
            writeCache(key, prev);
            writeTs(`${tsdir}/src/vars.ts`, key, prev);
            writeJs(`${tsdir}/src/vars.js`, key, prev);
            previous = appendPrev(previous, key, prev);

            console.log('');
            return true;
          } else {
            const inputDirty = isInputDirty(key, inputs);
            const cfpath = cfpath2.startsWith('/') ? cfpath2 : `${__dirname}/../../../node_modules/@inf/${name}/${cfpath2}`;
            if (!fs.existsSync(cfpath)) {
              error(`invalid cf module ${name} - ${cfpath} not found`, 14);
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

            const outs = (outputs || []).map(o => typeof o == 'string' ? o : o.name);
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
            writeIgnore(`${tsdir}/.gitignore`, ['vars.env', 'vars.ts', 'vars.js', 'src/vars.env', 'src/vars.ts', 'src/vars.js', 'lib']);

            console.log('(updated)');
            return true;
          }
        },

        shell => new Promise((resolve, reject) => {
          const { name, command, args, env, dependsOn, cwd, outputs } = shell;

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

              let json: { [_: string]: string } | undefined = undefined;
              try { json = cleanJson(JSON.parse(buffer.trim())); } catch (ex) { }
              if (json) {
                buffer = '';
                printOutputs(key, json);
                writeCache(key, json);
                previous = appendPrev(previous, key, json);
              } else {
                if (outputs) {
                  entries(outputs).forEach(([k, v]) => {
                    if (typeof v == 'string') {
                      if (!json) json = {};
                      json[k] = v;
                    } else {
                      const match = (v.outputMatcher as RegExp).exec(buffer);
                      if (match && match.length > 1) {
                        if (!json) json = {};
                        json[k] = match[1];
                      }
                    }
                  });
                }

                buffer = '';
                if (json) {
                  if (!fs.existsSync(tsdir))
                    fs.mkdirSync(tsdir);
                  writeEnvs(`${tsdir}/vars.env`, key, json);
                  writeTs(`${tsdir}/vars.ts`, key, json);
                  writeJs(`${tsdir}/vars.js`, key, json);

                  writeIgnore(`${tsdir}/.gitignore`, ['vars.env', 'vars.ts', 'vars.js', 'src/vars.env', 'src/vars.ts', 'src/vars.js', 'lib']);
                  printOutputs(key, json);
                  writeCache(key, json);
                  previous = appendPrev(previous, key, json);
                } else {
                  printOutputs(key, undefined);
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
      )(rec.record('CREATE', { configurationDir }));

      if (ret == undefined) {
        error(`record match failed: ${JSON.stringify(rec, null, 2)}`, 15);
        process.exit(1);
      }

    }
  } else if (cmd == 'down') {
    error('down not yet implemented');
    /*
    let previous = {};
    const ms = configuration.modules.map(f => {
      const n = typeof f == 'function' ? f(() => '') : f;
      const ret = parseModule(f, previous);
      previous = {};
      return ret;
    });
    for (let i = ms.length; i > 0; --i) {
      const m = ms[i - 1];
      if (m) await down(getStackname(m.name));
    }
    */
  } else {
    error(`unknown configure command '${cmd}'`, 16);
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
  console.error(err);
  error(err && err.message || err, 17);
  process.exit(1);
});
