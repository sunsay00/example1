import * as AWS from 'aws-sdk';
import * as fs from 'fs';

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
}


const writeResult = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `${key.toUpperCase()}_${k}=${v}\n`;
  });
  fs.writeFileSync(outPath, out);
};

const writeTs = (outPath: string, key: string, data: { [k: string]: string }) => {
  let out = '';
  Object.entries(data).map(([k, v]) => {
    out += `  ${k}: ${JSON.stringify(v)},\n`;
  });
  fs.writeFileSync(outPath, `export const outputs = {\n${out}};`);
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

  const up = async (StackName: string, cfPath: string, inputs: { [k: string]: string }, expectedOutputs: string[]) => {
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
    let [f, ...rest] = configuration.modules;
    while (true) {
      const m = parseModule(f, previous);
      if (m == undefined) break;
      const [nf, ...nrest] = rest;
      f = nf;
      rest = nrest;
      if (m.type == 'cloudformation') {
        if (!m.name || typeof m.name != 'string')
          throw new Error('invalid cloudformation module - missing/bad name');
        verifyKey(m.key);
        const tspath = `${__dirname}/../../node_modules/${m.name}/index.ts`;
        const envsdir = `${__dirname}/../../.envs`;
        if (!fs.existsSync(envsdir))
          fs.mkdirSync(envsdir);
        const opath = `${envsdir}/cf.${m.key.toLowerCase()}`;
        const cfpath = `${__dirname}/../../node_modules/${m.name}/cf.yaml`;
        const ot = lastmod(opath);
        if (lastmod(cfpath) <= ot && lastmod(configPath) <= ot) {
          log(`${m.name} is up to date`);
          continue;
        }
        if (!fs.existsSync(cfpath))
          throw new Error(`invalid cf module ${m.name} - ${cfpath} not found`);
        previous = await up(getStackname(m.name), cfpath, m.inputs, m.outputs);
        writeResult(opath, m.key, previous);
        writeTs(tspath, m.key, previous);
      } else if (m.type == 'shell') {
        console.log('shell command NYI', m);
      } else {
        throw new Error(`unknown type '${m.type}'`);
      }
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
