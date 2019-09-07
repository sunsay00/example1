// @ts-ignore
import * as V from '../../vars';
import { substitute } from '@inf/common';

export const substituteVariables = (vars: { [_: string]: string | undefined }) => (str: string) =>
  substitute(/({{([a-zA-Z0-9_]+)}})/gm, vars)(str);

export const vartools = {
  // @ts-ignore
  expand: substituteVariables({ ...process.env, ...V._vars }),
  convertToShell: (str: string) => str.replace(/({{([a-zA-Z0-9_]+)}})/gm, '${$2}'),
  convertToProcessEnv: (str: string) => str.replace(/({{([a-zA-Z0-9_]+)}})/gm, '${process.env.$2}'),
};
