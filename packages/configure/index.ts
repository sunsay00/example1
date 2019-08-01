import * as AWS from 'aws-sdk';
import * as fs from 'fs';
import { spawn } from 'child_process';
import * as RT from 'runtypes';
import { vars } from 'vars';

const CFRecord = RT.Record({
  type: RT.Literal('cloudformation'),
  key: RT.String.withConstraint(s => verifyKey(s)),
  name: RT.String,
}).And(RT.Partial({
  inputs: RT.Dictionary(RT.Union(RT.String, RT.Number)),
  outputs: RT.Array(RT.String),
}));

const ShellRecord = RT.Record({
  type: RT.Literal('shell'),
  key: RT.String.withConstraint(s => verifyKey(s)),
  cwd: RT.String,
  command: RT.String,
  args: RT.Array(RT.String),
}).And(RT.Partial({
  inputs: RT.Dictionary(RT.Union(RT.String, RT.Number)),
  outputs: RT.Array(RT.String),
  outfile: RT.String,
}));

const Record = RT.Union(CFRecord, ShellRecord);

type Record = RT.Static<typeof Record>;

export type Configuration = {
  region: string,
  stage: string,
  modules: Record[],
};

const error = (msg: string) => console.error(`[CONF] error: ${msg}`);
const log = (msg: string) => console.log(`[CONF] ${msg}`);

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

const _keys = {};
const verifyKey = (key: string) => {
  if (!/^[A-Z0-9]+$/.exec(key))
    throw new Error(`invalid key '${key}' - only uppercase letters and digits allowed`);
  if (_keys[key])
    throw new Error(`duplicate key '${key}'`);
  _keys[key] = true;
  return true;
}


const writeResult = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += key ? `${key.toUpperCase()}_${k}=${v}\n` : `${k}=${v}\n`;
  });
  fs.writeFileSync(outPath, out);
};

const writeTs = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `// this file has been automatically generated\n\nexport const vars = {\n${out}};`);
};

const main = async (cmd: string) => {
  const configPath = 'configuration.ts';
  if (!fs.existsSync(configPath)) {
    log('no configuration.ts found, exiting...');
    process.exit(1);
  }

  const { default: configuration } = await import('../../configuration');

  if (!configuration.region) {
    error('region not set');
    process.exit(1);
  }

  if (!configuration.stage) {
    error('stage not set');
    process.exit(1);
  }
  if (!['dev', 'beta', 'production'].includes(configuration.stage)) {
    error('invalid stage value - must be one of <dev|beta|production>');
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
        'UPDATE_ROLLBACK_IN_PROGRESS', 'UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS', 'REVIEW_IN_PROGRESS'].includes(s.StackStatus))
        throw new Error(`Stack busy: ${s.StackStatus}`);
      if (s.StackStatus == 'ROLLBACK_COMPLETE')
        throw new Error(`Stack '${StackName}' must be deleted manually`);
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
    if (missinginputs.length > 0)
      throw new Error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`);

    const Parameters = Object.entries(inputs).map(([k, v]) => ({ ParameterKey: k, ParameterValue: `${v}` }));
    const TemplateBody = fs.readFileSync(cfPath, 'utf8');
    const exists = await stackExists(StackName);
    try {
      if (exists) {
        log(`updating stack ${configuration.region} ${StackName}...`);
        await cf.updateStack({
          StackName,
          TemplateBody,
          Capabilities: ['CAPABILITY_NAMED_IAM'],
          Parameters
        }).promise();

        await cf.waitFor('stackUpdateComplete', { StackName }).promise();
      } else {
        log(`creating stack ${configuration.region} ${StackName}...`);
        await cf.createStack({
          StackName,
          TemplateBody,
          Capabilities: ['CAPABILITY_NAMED_IAM'],
          Parameters
        }).promise();

        await cf.waitFor('stackCreateComplete', { StackName }).promise();
      }
    } catch (err) {
      if (!err.message.includes('No updates are to be performed'))
        throw err;
    }

    const desc = await cf.describeStacks({ StackName }).promise();
    if (desc.Stacks.length != 1) throw new Error('failed to describe stacks');

    const outputs = desc.Stacks[0].Outputs;
    const missing = [];
    for (let eo in expectedOutputs)
      if (!outputs[eo])
        missing.push(eo);
    if (missing.length > 0)
      throw new Error(`missing outputs (${missing.join(', ')})`);
    const unused = [];
    for (let o in outputs)
      if (!expectedOutputs)
        unused.push(o);
    if (unused.length > 0)
      log(`unused outputs (${unused.join(', ')})`);
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

  const getStackname = name => `${configuration.stage}-${name}`;

  if (cmd == 'up') {
    let previous = {};
    for (let f of configuration.modules) {
      const mod = typeof f == 'function' ? (f as any)(previous) : f;
      await Record.match(
        async cloudformation => {
          const { name, key, inputs, outputs } = cloudformation;
          const tsdir = `${__dirname}/../../node_modules/${name}/src`;
          if (!fs.existsSync(tsdir))
            fs.mkdirSync(tsdir);
          const envsdir = `${__dirname}/../../.envs.${configuration.stage}`;
          if (!fs.existsSync(envsdir))
            fs.mkdirSync(envsdir);
          const opath = `${envsdir}/cf.${key.toLowerCase()}`;
          const cfpath = `${__dirname}/../../node_modules/${name}/cf.yaml`;
          const ot = lastmod(opath);
          if (lastmod(cfpath) <= ot && lastmod(configPath) <= ot) {
            log(`${name} is up to date`);
            return false;
          }
          if (!fs.existsSync(cfpath))
            throw new Error(`invalid cf module ${name} - ${cfpath} not found`);
          previous = await up(getStackname(name), cfpath, inputs, outputs);
          writeResult(opath, key, previous);
          writeTs(`${tsdir}/vars.ts`, key, previous);
          return false;
        },
        shell => new Promise<boolean>((resolve, reject) => {
          let dirty = true;
          let outputs = false;
          let mode = '';
          previous = {};
          const { command, args, cwd, outfile } = shell;
          const proc = spawn(command, args, { env: vars, cwd });
          proc.stdout.on('data', data => {
            const str = data.toString().trim();
            if (!outputs) {
              if (command == 'make' && /^make.*: Nothing to be done for/.exec(str))
                dirty = false;
              else if (/^Service Information/.exec(str))
                outputs = true;
            } else {
              if (/^Serverless: Run/.exec(str)) {
                outputs = false;
              } else {
                const sexec = /^(.+):/.exec(str);
                if (sexec && sexec.length == 2)
                  mode = sexec[1];
                if (mode == 'endpoints') {
                  const exec = /POST - (.+\/)(.+)/.exec(str);
                  if (exec && exec.length == 3)
                    previous[`${exec[2]}Endpoint`] = `${exec[1]}${exec[2]}`;
                }
              }
            }
            process.stdout.write(data.toString());
          });
          proc.stderr.on('data', data => process.stderr.write(data.toString()));
          proc.on('close', code => {
            if (code == 0 && dirty && outfile) {
              const opath = `${__dirname}/../../${cwd}/${outfile}`;
              writeResult(opath, '', previous);
            }
            code != 0 ? reject(new Error('shell failed')) : resolve(false);
          });
        })
      )(mod);
    }
  } else if (cmd == 'down') {
    let previous = {};
    const ms = configuration.modules.map(f => {
      const ret = parseModule(f, previous);
      const p = {};
      if (f.outputs && Array.isArray(f.outputs))
        f.outputs.forEach(o => p[o] = true);
      previous = p;
      return ret;
    });
    for (let i = ms.length; i > 0; --i) {
      const m = ms[i - 1];
      if (m) await down(getStackname(m.name));
    }
  } else {
    throw new Error(`unknown configure command '${cmd}'`);
  }
};

if (process.argv.length != 3) {
  console.log(`usage: configure <up|down>`);
  process.exit(1);
}

const cmd = process.argv[2];
main(cmd).catch(err => {
  error(err && err.message || err);
  process.exit(1);
});
