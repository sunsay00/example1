import { useState, useEffect } from 'react';
import { map, fromEntries, keys } from 'common';

type FormFieldType = 'email' | 'username' | 'text' | 'password' | 'unsigned';

type FormResultType<T extends FormFieldType> =
  T extends 'email' ? string :
  T extends 'username' ? string :
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
    placeholder?: string,
    pattern?: RegExp,
    message?: string,
    default: FormResultTypes<T>[k],
  }
};

const isValid = (value: string, type: FormFieldType, pattern?: RegExp) => {
  if (pattern) {
    return value.match(pattern) != null;
  } else {
    if (type == 'email') {
      return value.match(/^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i) != null;
    } else if (type == 'username') {
      return value.match(/^[a-zA-Z0-9_-]{2,30}$/) != null;
    } else {
      return true;
    }
  }
}

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

const cleanFormResult = <T extends unknown>(type: FormFieldType, result: T): T => {
  if (type == 'unsigned' && typeof result == 'number') {
    return result;
  } else if (typeof result == 'string') {
    return result.trim() as T;
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
  }

  useEffect(() => {
    reset();
  }, []);

  return {
    fields: fromEntries(map(keys(opts), k => [k, {
      value: (states[k].result as FormResultTypes<T>[typeof k] | undefined) || opts[k].default as FormResultTypes<T>[typeof k],
      message: hasValidated && !states[k].valid && opts[k].message || undefined,
      placeholder: opts[k].placeholder || '',
      testID: (opts[k].placeholder || '').toUpperCase().replace(/ /g, '_'),
      onChangeText: (value: string) => {
        const opt = opts[k];
        const valid = isValid(value, opt.type, opt.pattern);
        setStates(state => {
          const prev = state[k];
          const prevvalue = (prev.result as FormResultTypes<T>[typeof k] | undefined) || opt.default as FormResultTypes<T>[typeof k];
          const result = parseFormValue<FormResultTypes<T>[typeof k]>(prev.type, value, prevvalue);
          const next = { ...prev, valid, value, result };
          return { ...state, [k]: next };
        });
      }
    }] as const)),
    submit: () => {
      setHasValidated(true);
      for (let k in states)
        if (!states[k].valid || states[k].result == undefined)
          return;
      const ret = mapobj(states, f => cleanFormResult(f.type, f.result!));
      onSubmit(ret as FormResultTypes<T>)
        .then(reset);
    },
  };
}