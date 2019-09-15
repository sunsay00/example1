export type Getter<R> = {
  promise: () => Promise<R>,
  current: () => R | undefined,
};

export const createGetter = <R>(p: Promise<R>, onReady?: (r: R) => void): Getter<R> => {
  let ready = false;
  let outbox: ((ret: R) => void)[] = [];
  let result: R | undefined;
  const ret = {
    current: () => result,
    promise: () => new Promise<R>((yes, no) => {
      if (!ready) {
        ready = true;
        p.then(r => {
          result = r;
          const o = outbox;
          outbox = [];
          o.forEach(fn => fn(r));
          onReady && onReady(r);
        }).catch(err => {
          console.error(err);
          no(err);
        });
      }
      result != undefined ? yes(result) : outbox.push(yes);
    }),
  };
  return ret;
};
