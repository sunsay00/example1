import { useState, useEffect, useRef } from 'react';
import { Shared } from '../tools/createshared';
import { createCanceler } from '../tools/canceler';

type UpdateFn<R> = (ctorFn: () => Promise<R>, dtorFn?: ((t: R) => Promise<void>) | undefined) => Promise<boolean> | undefined | boolean;

export const useShared = <R>(shared: Shared<R>, updateFn: (shared: UpdateFn<R>) => Promise<boolean> | undefined | boolean, inputs?: any[]) => {

  const [value, setValue] = useState<R | undefined>(shared.value());
  const updateFnRef = useRef(updateFn);
  updateFnRef.current = updateFn;

  useEffect(() => {
    shared.addRef();
    return () => {
      shared.remRef();
    };
  }, []);

  useEffect(() => {
    const cancel = createCanceler();
    const ret = updateFnRef.current((ctor, dtor) => shared.update(ctor, dtor, inputs))
    typeof ret != 'boolean' && ret && cancel.add(ret)
      .then(needsUpdate => needsUpdate && setValue(shared.value()))
      .catch(console.error);
    return () => cancel.cancel();
  }, inputs);

  return value;
};
