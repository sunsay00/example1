import { Result, to } from './result';

interface Canceler {
  add: <T, _ = never>(promise: Promise<T>) => Promise<T>;
  add_to: <T, _ = never>(promise: Promise<T>) => Promise<Result<T>>;
  cancel(): void;
}

class BasicCanceler implements Canceler {
  private _fns: [any, () => void][] = [];
  _push = (owner: any, fn: () => void) => this._fns.push([owner, fn]);
  _remove = (owner: any) => this._fns = this._fns.filter(f => f[0] != owner);
  add<T>(promise: Promise<T>) {
    const thiz = this;
    let cancelled = false;
    this._push(thiz, () => cancelled = true);
    return new Promise<T>((resolve, reject) => {
      promise.then(val => cancelled ? this._remove(thiz) : resolve(val), error => cancelled ? this._remove(thiz) : reject(error));
    });
  };
  add_to<T>(promise: Promise<T>) { return to(promise); };
  cancel = () => { this._fns.forEach(t => t[1]()); this._fns = []; }
};

export const createCanceler = () => new BasicCanceler();