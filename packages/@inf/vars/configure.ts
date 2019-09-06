import * as AWS from 'aws-sdk';
import * as fs from 'fs';
import { spawn, execSync } from 'child_process';
import * as RT from 'runtypes';
import * as crypto from 'crypto';
import * as mm from 'micromatch';
import * as path from 'path';
import * as colors from 'colors/safe';
import { fromEntries, entries, Diff, substitute, JSONArray, JSONValue } from '@inf/common';

const getJSONHash = (data: JSONValue) => {
  const shasum = crypto.createHash('sha1');
  const recur = (x: JSONValue) => {
    if (x == null) {
      shasum.update('[null]');
    } else if (x instanceof Array) {
      shasum.update('[array]');
      x.forEach(i => recur(i));
    } else if (typeof x == 'object') {
      shasum.update('[object]');
      entries(x).sort((a, b) => a[0].localeCompare(b[0])).map(([k, v]) => [k, recur(v)]);
    } else {
      shasum.update(JSON.stringify(x));
    }
  };
  recur(data);
  return shasum.digest('hex');
}

export const configEffect = async (fn: () => Promise<void>, changes?: JSONArray) => {
  if (!changes) {
    await fn();
  } else {
    if (!fs.existsSync(`${__dirname}/.effects`))
      fs.mkdirSync(`${__dirname}/.effects`);
    const hash = getJSONHash(changes);
    const hashpath = `${__dirname}/.effects/${hash}`;
    if (!fs.existsSync(hashpath)) {
      await fn();
      fs.writeFileSync(hashpath, '', { encoding: 'utf8' });
    }
  }
}

export const makeStackname = (stage: string, rootid: string, id?: string) =>
  `${stage ? `${stage}-` : ''}${rootid}${id ? `--${id}` : ''}`;

const verifyRegEx = (value: unknown, errMessage: string) => {
  if (Object.prototype.toString.call(value) == '[object RegExp]')
    return true;
  error(colors.red(errMessage));
  throw new Error(errMessage);
}

const _moduleids: { [_: string]: boolean } = {};
const verifyModuleId = (s: string) => {
  if (!/^[a-zA-Z0-9-_:]+$/.exec(s)) {
    error(`invalid moduleid '${s}' - only letters, digits, dashes, colons, and underscores are allowed`, 1);
    throw new Error(`invalid moduleid '${s}' - only letters, digits, dashes, and underscores are allowed`);
  }
  if (s.includes('--')) {
    error(`invalid moduleid '${s}' - cannot have multiple sequential dashes`);
    throw new Error(`invalid moduleid '${s}' - cannot have multiple sequential dashes`);
  }
  if (_moduleids[s]) {
    error(`duplicate moduleid '${s}' detected`, 2);
    throw new Error(`duplicate moduleid '${s}' detected`);
  }
  _moduleids[s] = true;
  return true;
}

const moduleIdExists = (s: string): boolean => {
  return !!_moduleids[s];
}

const verifyId = (s: string) => {
  if (!/^[a-zA-Z0-9-_]+$/.exec(s)) {
    error(`invalid id '${s}' - only letters, digits, dashes, and underscores are allowed`, 1);
    throw new Error(`invalid id '${s}' - only letters, digits, dashes, and underscores are allowed`);
  }
  if (s.includes('--')) {
    error(`invalid id '${s}' - cannot have multiple sequential dashes`);
    throw new Error(`invalid id '${s}' - cannot have multiple sequential dashes`);
  }
  return true;
}

const CloudFormationRecord = RT.Record({
  type: RT.Literal('cloudformation'),
  cfpath: RT.String,
}).And(RT.Partial({
  id: RT.String.withConstraint(s => verifyId(s)),
  vars: RT.Dictionary(RT.String),
  inputs: RT.Dictionary(RT.Union(RT.String, RT.Number, RT.Array(RT.String))),
}));

const ReplaceVarsRecord = RT.Record({
  type: RT.Literal('replace-vars'),
  targetModuleId: RT.String.withConstraint(s => moduleIdExists(s)),
}).And(RT.Partial({
  id: RT.String.withConstraint(s => verifyId(s)),
  vars: RT.Dictionary(RT.String),
}));

const ShellRecord = RT.Record({
  type: RT.Literal('shell'),
  command: RT.String,
  args: RT.Array(RT.String),
}).And(RT.Partial({
  id: RT.String.withConstraint(s => verifyId(s)),
  cwd: RT.String,
  dependsOn: RT.Array(RT.String),
  vars: RT.Dictionary(RT.String),
  env: RT.Dictionary(RT.String),
  outputMatchers: RT.Dictionary(RT.Unknown.withConstraint(s => verifyRegEx(s, 'outputMatchers must only contain regular expressions')))
}));

const Record = RT.Union(CloudFormationRecord, ShellRecord, ReplaceVarsRecord);

export type CFParams = { configurationDir: string, stage: string, region: string };

type CFRecord<R extends { [_: string]: string }> = RT.Static<typeof Record> & {
  rootDir: string,
  outputs: R,
  children?: (reg: Registerer) => Promise<void>
};

export type ConfigRecord<R extends { [_: string]: string }> = { up: (params: CFParams, reg: Registerer) => Promise<CFRecord<R>>, clean: (params: CFParams, reg: Registerer) => Promise<R> };
type ConfigRecordFn<R extends { [_: string]: string }> = (outputs: (k: string) => string) => ConfigRecord<R>;
export type ModuleRecord<R extends { [_: string]: string }> = ConfigRecordFn<R> | ConfigRecord<R>;
type Registerer = <R extends { [_: string]: string }>(record: ModuleRecord<R>) => Promise<R>;
export type ShellOutputMatchers = Diff<RT.Static<typeof ShellRecord>['outputMatchers'], undefined>;

export type Configuration = {
  region: string;
  stage: string;
  configure: (reg: Registerer) => Promise<void>
};

export const createConfig = <R extends { [_: string]: string }>(rc: ((params: CFParams) => ConfigRecord<R>) | ConfigRecord<R>) => ({
  up: (params: CFParams, reg: Registerer) => typeof rc == 'function' ? rc(params).up(params, reg) : rc.up(params, reg),
  clean: (params: CFParams, reg: Registerer) => typeof rc == 'function' ? rc(params).clean(params, reg) : rc.clean(params, reg),
});
export const createRecord = <R extends { [_: string]: string }>(rc: CFRecord<R> | ((params: CFParams, reg: Registerer) => Promise<CFRecord<R>>)): ConfigRecord<R> => ({
  up: async (params: CFParams, reg: Registerer) => {
    const rec = typeof rc == 'function' ? rc(params, reg) : rc;
    return await rec;
  },
  clean: async (params: CFParams, reg: Registerer) => {
    const rec = await (typeof rc == 'function' ? rc(params, reg) : rc);
    return rec.outputs;
  }
});
export const createConfigRecord = <R extends { [_: string]: string }>(rc: CFRecord<R>) =>
  createConfig<R>(createRecord<R>(rc));

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

const printOutputs = (key: string, json?: { [_: string]: string }) => {
  if (verbose && json) {
    const ents = Object.entries(json);
    if (ents.length > 0) {
      console.log('outputs:');
      ents.forEach(([k, v]) => console.log(`  ${key}_${k}=${v}`));
    }
  }
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

const expandInputVars = (inputs: { [_: string]: string | number | string[] }): { [_: string]: string | number | string[] } => {
  const expandString = substitute(/({{([a-zA-Z0-9_]+)}})/gm, process.env);
  const expand = (x: string | number | string[]) => {
    if (typeof x == 'string') {
      return expandString(x);
    } else if (typeof x == 'number') {
      return x;
    } else {
      return x.map(expandString);
    }
  }
  return fromEntries(entries(inputs).map(([k, v]) => [k, expand(v)]));
}

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

export const writeTmps = (name: 'outputs' | 'vars', tsdir: string, data: { [_: string]: string }) => {
  const writeEnvs = (outPath: string) => {
    let out = '';
    Object.entries(data).map(([k, v]) => {
      out += `${k}=${v}\n`;
      //out += `${k}=${v.replace(/({{([a-zA-Z0-9_]+)}})/gm, '${$2}')}\n`;
    });
    fs.writeFileSync(outPath, `# this file has been automatically generated\n\n${out}`);
  };

  const tsStr = (data: { [k: string]: string }) => {
    let out = '';
    Object.entries(data).sort((a, b) => a[0].localeCompare(b[0])).map(([k, v]) => {
      out += `  ${k}: \`${v.replace(/({{([a-zA-Z0-9_]+)}})/gm, '${process.env.$2}')}\`,\n`;
    });
    return `// this file has been automatically generated\n\nexport const ${name} = {\n${out}};`;
  };

  const writeTs = (outPath: string) =>
    fs.writeFileSync(outPath, tsStr(data));

  const readTmps = () => fs.readFileSync(`${tsdir}/src/_${name}.ts`, { encoding: 'utf8' });

  const equalTmps = () => {
    if (!fs.existsSync(`${tsdir}/src/_${name}.ts`)) return false;
    return readTmps() == tsStr(data);
  }

  const unlinkTmps = () => {
    if (fs.existsSync(`${tsdir}/src/_${name}.env`)) fs.unlinkSync(`${tsdir}/src/_${name}.env`);
    if (fs.existsSync(`${tsdir}/src/_${name}.ts`)) fs.unlinkSync(`${tsdir}/src/_${name}.ts`);
  }

  const writeGen = () => {
    const writeIndex = (outpath: string) => {
      const hasvars = fs.existsSync(`${tmpdir}/vars.ts`);
      const hasoutputs = fs.existsSync(`${tmpdir}/outputs.ts`);
      if (fs.existsSync(outpath)) fs.unlinkSync(outpath);
      const indexstr = `
${hasvars ? `export { vars } from './vars';` : ''}
${hasoutputs ? `export { outputs } from './outputs';` : ''}
`;
      fs.writeFileSync(outpath, `// this file has been automatically generated\n\n${indexstr}`, { encoding: 'utf8' });
    }

    const gendir = path.join(__dirname, 'gen');
    if (!fs.existsSync(gendir))
      fs.mkdirSync(gendir);
    const tmpdir = path.join(gendir, path.basename(tsdir));
    if (!fs.existsSync(tmpdir))
      fs.mkdirSync(tmpdir);

    writeTs(`${tmpdir}/${name}.ts`);
    if (name == 'vars')
      writeEnvs(`${tmpdir}/${name}.env`);

    writeIndex(`${tmpdir}/index.ts`);
  }

  //writeGen();

  if (equalTmps())
    return;

  unlinkTmps();

  if (!fs.existsSync(tsdir))
    fs.mkdirSync(tsdir);
  if (!fs.existsSync(`${tsdir}/src`))
    fs.mkdirSync(`${tsdir}/src`);

  writeTs(`${tsdir}/src/_${name}.ts`);
  writeEnvs(`${tsdir}/_${name}.env`);
}

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
        console.log(' updating... ');
        await cf.updateStack({
          StackName,
          TemplateBody,
          Capabilities: ['CAPABILITY_NAMED_IAM'],
          Parameters
        }).promise();
        await cf.waitFor('stackUpdateComplete', { StackName }).promise();
      } else {
        process.stdout.write(' creating... ');
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

  const writeIgnores = (tsdir: string) => {
    writeIgnore(`${tsdir}/.gitignore`, ['_vars.*', '_outputs.*']);
  }

  let previous: { [_: string]: string } = {};

  if (cmd == 'clean') {
    const tmpinputsdir = `${__dirname}/.inputs`;
    const tmpcachedir = `${__dirname}/.cache`;
    const tmpeffectsdir = `${__dirname}/.effects`;
    if (fs.existsSync(tmpinputsdir))
      execSync(`rm -rf ${tmpinputsdir}`);
    if (fs.existsSync(tmpcachedir))
      execSync(`rm -rf ${tmpcachedir}`);
    if (fs.existsSync(tmpeffectsdir))
      execSync(`rm -rf ${tmpeffectsdir}`);

    const reg = async <R extends { [_: string]: string }>(f: ModuleRecord<R>): Promise<R> => {
      const rec: ConfigRecord<R> = typeof f == 'function' ? f(k => previous[k]) : f;
      return await rec.clean({ configurationDir, stage: configuration.stage, region: configuration.region }, reg);
    };
    await configuration.configure(reg);

  } else if (cmd == 'up') {

    const reg = (depth: number) => async <R extends { [_: string]: string }>(f: ModuleRecord<R>) => {
      const rec = typeof f == 'function' ? f(k => {
        const ret = previous[k];
        if (!ret) {
          error(`invalid output variable '${k}'`, 13);
          throw new Error(`invalid output variable '${k}'`);
        }
        return ret;
      }) : f;

      const record = await rec.up({ configurationDir, stage: configuration.stage, region: configuration.region }, reg(depth + 1));
      const rootid = path.basename(record.rootDir);

      const key = rootid.replace(/-/g, '_').toUpperCase();

      const id = record.id || '';

      const showlog = depth == 0;

      const moduleid = `${rootid}${record.id ? `:${record.id}` : ''}`;
      verifyModuleId(moduleid);
      /*showlog &&*/ log(`${'  '.repeat(depth)}${depth > 0 ? colors.grey(moduleid) : moduleid}`);

      const stackName = makeStackname(configuration.stage, rootid, id);

      const tsdir = path.resolve(record.rootDir);

      let result: any = undefined;
      const ret = await Record.match(

        async cloudformation => {
          const { inputs, cfpath: cfpath2, vars } = cloudformation;

          const expandedInputs = inputs && expandInputVars(inputs);

          if (configuration.stage == 'local') {
            const prev: { [_: string]: string } = {};
            record.outputs && Object.entries(record.outputs).forEach(([key, o]) => !o ? prev[key] = 'LOCAL_UNUSED' : prev[key] = o);
            vars && writeTmps('vars', tsdir, vars);
            writeCache(key, prev);
            writeIgnores(tsdir);

            result = prev;
            previous = appendPrev(previous, key, prev);

            return true;
          } else {
            const inputDirty = isInputDirty(key, expandedInputs);
            const cfpath = cfpath2.startsWith('/') ? cfpath2 : `${tsdir}/${cfpath2}`;
            if (!fs.existsSync(cfpath)) {
              error(`invalid cf module ${moduleid} - ${cfpath} not found`, 14);
              throw new Error(`invalid cf module ${moduleid} - ${cfpath} not found`);
            }

            const ot = lastmod(`${__dirname}/.cache/${key}`);
            const cfDirty = lastmod(cfpath) > ot;
            if (!cfDirty && !inputDirty) {
              const prev = readCache(key);
              if (prev) {

                result = prev;
                previous = appendPrev(previous, key, prev);

                return true;
              }
            }

            vars && writeTmps('vars', tsdir, vars);
            const expectedOutputs = record.outputs ? Object.entries(record.outputs).map(([k, _]) => k) : [];
            const prev = await up(stackName, cfpath, expandedInputs, expectedOutputs);
            writeCache(key, prev);
            writeInput(key, expandedInputs);

            result = prev;
            previous = appendPrev(previous, key, prev);

            writeTmps('outputs', tsdir, prev);
            writeIgnores(tsdir);

            showlog && process.stdout.write('(updated)');

            return true;
          }
        },

        shell => new Promise((resolve, reject) => {
          const { command, args, env, dependsOn, cwd, outputMatchers, vars } = shell;

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

              result = prev;
              previous = appendPrev(previous, key, prev);

              showlog && process.stdout.write('');
              resolve(true);
              return;
            }
          }

          vars && writeTmps('vars', tsdir, vars);

          const cwdEnabled = !command.startsWith('.') && !command.startsWith('/') && cwd;
          const cwd2 = cwdEnabled ? cwd : path.dirname(command);
          const cmd = cwdEnabled ? command : path.basename(command);
          const proc = spawn(cmd, args, { env: { ...process.env, ...env }, cwd: cwd2 });
          let buffer = '';
          proc.stdout.on('data', data => {
            const buf = data.toString();
            if (verbose) {
              if (!buffer.length) showlog && console.log('');
              process.stdout.write(colors.gray(buf));
            }
            buffer += buf;
          });
          proc.stderr.on('data', data => process.stderr.write(data.toString()));
          proc.on('close', code => {
            if (code == 0) {

              if (verbose && buffer.length > 0)
                showlog && console.log('');

              let json: { [_: string]: string } | undefined = undefined;
              try { json = cleanJson(JSON.parse(buffer.trim())); } catch (ex) { }
              if (json) {
                if (record.outputs) {
                  for (let k in record.outputs)
                    if (json[k] == undefined)
                      throw new Error(`${moduleid} is missing output '${k}'`);
                }
                buffer = '';
                printOutputs(key, json);
                writeCache(key, json);

                result = json;
                previous = appendPrev(previous, key, json);
              } else {
                if (record.outputs)
                  Object.entries(record.outputs).forEach(([k, v]) => {
                    if (!json) json = {};
                    json[k] = v;
                  })

                if (outputMatchers) {
                  entries(outputMatchers).forEach(([k, v]) => {
                    if (typeof v == 'string') {
                      if (!json) json = {};
                      json[k] = v;
                    } else {
                      const match = (v as RegExp).exec(buffer);
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

                  writeTmps('outputs', tsdir, json);
                  writeIgnores(tsdir);

                  printOutputs(key, json);
                  writeCache(key, json);

                  result = json;
                  previous = appendPrev(previous, key, json);
                } else {
                  printOutputs(key, undefined);
                  writeCache(key, prev);

                  result = prev;
                  previous = appendPrev(previous, key, prev);
                }
              }


              showlog && process.stdout.write('(updated)');
              resolve(true);
            } else {
              reject(new Error('shell failed'));
            }
          });
        }),

        replacevars => new Promise(resolve => {
          const { vars } = replacevars;

          vars && writeTmps('vars', tsdir, vars);

          const prev = {};
          printOutputs(key, undefined);
          result = prev;
          previous = appendPrev(previous, key, prev);

          resolve(true);
        }),

      )(record);

      if (ret == undefined) {
        error(`record match failed: ${JSON.stringify(record, null, 2)}`, 15);
        process.exit(1);
      }

      /*showlog &&*/ console.log('');

      return result;
    };
    await configuration.configure(reg(0));

  } else if (cmd == 'down') {
    error('down not yet implemented');
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
