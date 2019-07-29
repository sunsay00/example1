import { useState } from 'react';

type FormFieldType = 'text' | 'password' | 'unsigned';

type FormResultType<T extends FormFieldType> =
  T extends 'text' ? string :
  T extends 'password' ? string :
  T extends 'unsigned' ? number :
  unknown;

type FormType<T> = T extends { type: infer U } ? (U extends FormFieldType ? U : never) : never;

type FormResultTypes<T> = { [k in keyof T]: FormResultType<FormType<T[k]>> };

type FormStates<T> = {
  [k in keyof T]: {
    type: FormFieldType,
    value: string,
    valid: boolean,
    result: FormResultTypes<T>[k] | undefined
  }
};

type FormFields<T> = {
  [k in keyof T]: {
    type: FormFieldType,
    pattern?: RegExp,
    message?: string,
    default: FormResultTypes<T>[k],
  }
};

const parseFormValue = <T extends unknown>(type: FormFieldType, val: string, prev: T): T => {
  if (type == 'unsigned' && typeof prev == 'number') {
    try {
      return parseInt(val) as T;
    } catch (err) {
      return prev;
    }
  } else if (typeof prev == 'string') {
    return val as T;
  } else {
    throw new Error('failed to parse value')
  }
}

export const mapobj = <T, U, V, K extends keyof T>(obj: { [k in K]: U }, fn: (v: U) => V): { [k in K]: V } => {
  const ret = {} as { [k in K]: V };
  for (let k in obj) ret[k] = fn(obj[k]);
  return ret;
}

export const useForm = <T extends FormFields<T>, L extends keyof T>(opts: T, onSubmit: (results: FormResultTypes<T>) => Promise<void>) => {
  const [hasValidated, setHasValidated] = useState(false);
  const [states, setStates] = useState<FormStates<T>>(mapobj(opts as FormFields<T>, f => ({
    type: f.type,
    value: '',
    valid: false,
    result: undefined
  })) as FormStates<T>);

  const reset = () => {
    setHasValidated(false);
    setStates(mapobj(opts, x => ({
      value: '',
      valid: false,
      result: undefined,
    })) as FormStates<T>);
  };

  return {
    reset,
    value: <L extends keyof T, R extends FormResultTypes<T>[L]>(k: L): FormResultTypes<T>[L] =>
      (states[k].result as R | undefined) || opts[k].default as R,
    message: (k: L) => hasValidated && !states[k].valid && opts[k].message || undefined,
    changeText: <L extends keyof T, R extends FormResultTypes<T>[L]>(k: L) => (value: string) => {
      const opt = opts[k];
      const valid = !opt.pattern || value.match(opt.pattern) != null;
      setStates(state => {
        const prev = state[k];
        const prevvalue = (prev.result as R | undefined) || opt.default as R;
        const result = parseFormValue<R>(prev.type, value, prevvalue);
        const next = { ...prev, valid, value, result };
        return { ...state, [k]: next };
      });
    },
    submit: () => {
      setHasValidated(true);
      for (let k in states)
        if (!states[k].valid || states[k].result == undefined)
          return;
      const ret = mapobj(states, f => f.result!);
      onSubmit(ret as FormResultTypes<T>);
    }
  };
}