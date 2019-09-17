import * as path from 'path';
import * as fs from 'fs';
import * as AWS from 'aws-sdk';
import { entries, fromEntries } from '@inf/common';
import { useUniqueIdAssertion, useGlobals, useDependsOn, useCache, vartools } from '@inf/hookops';
import { vars } from '@inf/hookops/vars';

const expandInputVars = (props: { [_: string]: string | number | string[] }): { [_: string]: string | number | string[] } => {
  const expand = (x: string | number | string[]) => {
    if (typeof x == 'string') {
      return vartools.expand(x);
    } else if (typeof x == 'number') {
      return x;
    } else {
      return x.map(vartools.expand);
    }
  }
  return fromEntries(entries(props).map(([k, v]) => [k, expand(v)]));
}

const down = async (cf: AWS.CloudFormation, StackName: string) => {
  await cf.deleteStack({ StackName }).promise();
  await cf.waitFor('stackDeleteComplete', { StackName }).promise();
}

const up = async (cf: AWS.CloudFormation, StackName: string, cfPath: string, inputs: { [k: string]: string | number | string[] } | undefined, expectedOutputs: string[]) => {
  const stackExists = async (cf: AWS.CloudFormation, StackName: string) => {
    try {
      const stacks = await cf.describeStacks({
        StackName
      }).promise();
      if (!stacks.Stacks || stacks.Stacks.length != 1) return false;
      const s = stacks.Stacks[0];
      if (['CREATE_IN_PROGRESS', 'ROLLBACK_IN_PROGRESS', 'DELETE_IN_PROGRESS', 'DELETE_FAILED', 'UPDATE_IN_PROGRESS', 'UPDATE_COMPLETE_CLEANUP_IN_PROGRESS',
        'UPDATE_ROLLBACK_IN_PROGRESS', 'UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS', 'REVIEW_IN_PROGRESS'].includes(s.StackStatus)) {
        throw new Error(`Stack busy: ${s.StackStatus}`);
      }
      if (s.StackStatus == 'ROLLBACK_COMPLETE') {
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
  };

  const expandedInputs = inputs && expandInputVars(inputs);
  const missinginputs = [];
  for (let m in inputs)
    if (!inputs[m])
      missinginputs.push(m);

  if (missinginputs.length > 0)
    throw new Error(`missing inputs (${missinginputs.join(', ')}) for ${StackName}`);

  const Parameters = expandedInputs && entries(expandedInputs).map<AWS.CloudFormation.Parameter>(([k, v]) => ({
    ParameterKey: k,
    ParameterValue: Array.isArray(v) ? `${v.map(i => `${i}`).join(',')}` : `${v}`
  }));
  const TemplateBody = fs.readFileSync(cfPath, 'utf8');
  const exists = await stackExists(cf, StackName);
  try {
    if (exists) {
      await cf.updateStack({
        StackName,
        TemplateBody,
        Capabilities: ['CAPABILITY_NAMED_IAM'],
        Parameters
      }).promise();
      await cf.waitFor('stackUpdateComplete', { StackName }).promise();
    } else {
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
      throw err;
    }
  }

  const desc = await cf.describeStacks({ StackName }).promise();
  if (!desc.Stacks || desc.Stacks.length != 1) {
    throw new Error('failed to describe stacks');
  }

  const outputs = desc.Stacks[0].Outputs || [];
  const missing = [];
  for (let eo in expectedOutputs)
    if (!outputs[eo])
      missing.push(eo);
  if (missing.length > 0) {
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

export const useCloudFormation = async<R>(inputs: {
  id: string,
  cfyamlpath: string,
  inputs?: {
    [_: string]: string | number | string[]
  },
  defaultOutputs?: { [_ in keyof R]: string }
}): Promise<{ [_ in keyof R]: string }> => {

  const { stage, currentModuleDir: currentModuleDir } = useGlobals();

  useUniqueIdAssertion('cloudformation', inputs.id);

  const cf = new AWS.CloudFormation({ apiVersion: '2010-05-15', region: vars.AWS_REGION });

  if (stage == 'local') {
    if (inputs.defaultOutputs)
      return fromEntries(entries(inputs.defaultOutputs).map(([key, o]) => [key, !o ? 'LOCAL_UNDEFINED' : o]));
    else
      return {} as { [_ in keyof R]: string };
  } else {
    let dirty = false;

    await useDependsOn(async () => {
      dirty = true;
    }, [inputs.cfyamlpath]);

    return await useCache(async () => {
      const absRootDir = path.resolve(currentModuleDir);
      const cfpath = inputs.cfyamlpath.startsWith('/') ? inputs.cfyamlpath : `${absRootDir}/${inputs.cfyamlpath}`;
      if (!fs.existsSync(cfpath)) {
        throw new Error(`invalid cloudformation id '${inputs.id}' - ${cfpath} not found`);
      }

      const expectedOutputs = inputs.defaultOutputs ? Object.entries(inputs.defaultOutputs).map(([k, _]) => k) : [];

      const stackName = `${stage}-${inputs.id}`;
      const results = await up(cf, stackName, cfpath, inputs.inputs, expectedOutputs);

      return results as { [_ in keyof R]: string };
    }, dirty);
  }
}
