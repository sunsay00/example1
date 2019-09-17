export type Result<T> = { __result: boolean, err?: any, ok: T }

export const fn = <R>(f: () => void | Promise<R>): Promise<Result<R>> => new Promise<Result<R>>(async (y, n) => {
  try {
    const x = f();
    if (typeof x == 'object') { y(ok(await x)); }
  } catch (err) {
    //if (err instanceof _Err) { throw err; } else
    y(er(err));
  }
});

export function to<T, U = never>(promise: Promise<T>): Promise<Result<T>>;
export function to<T>(promise: T | Promise<T>): Result<T> | Promise<Result<T>> {
  if (promise && (promise as any).then != undefined) {
    return (promise as Promise<T>).then(k => ok(k)).catch(err => er<T>(err));
  } else {
    try {
      return ok(promise as T);
    } catch (err) {
      return er(err);
    }
  }
}
export const ok = <T, U = never>(v: T): Result<T> => ({ __result: true, ok: v });
export const er = <T, U = never>(v: any): Result<T> => ({ __result: false, ok: null as any, err: v });
