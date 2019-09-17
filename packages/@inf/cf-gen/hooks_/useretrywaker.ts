import { createShared } from '../tools_/createshared';
//import Root from '../root';
import { useShared } from './useshared';
import { ASSERT } from '../tools_/assert';

const DEBUG_RETRY = process.env.NODE_ENV == 'local';
const VERBOSE = false;

const _debugSet = {} as { [_: string]: number };
const _debugAdd = (key: string) => { const n = _debugSet[key] || 0; _debugSet[key] = n + 1; _debugPrint('ADD'); }
const _debugRem = (key: string) => { const n = _debugSet[key]; if (n == undefined) console.warn('debug run underflow'); else _debugSet[key] = n - 1; _debugPrint('REM'); }
const _debugPrint = (prefix?: string) => { console.log(`]=>    ${prefix || ''} RETRY SET: [${Object.entries(_debugSet).filter(([k, v]) => v && v > 0).map(([k, v]) => k).join(', ')}]`); }

class RetryWaker {
  private _retryOutbox: ((cancel: boolean) => void)[] = [];
  _shutdown = () => {
    if (this._retryOutbox.length > 0) {
      console.log(`RetryWaker cleanup ${this._retryOutbox.length}`);
      this._retryOutbox.forEach(fn => fn(true));
      ASSERT(this._retryOutbox.length == 0, 'RetryWaker shutdown - retryOutput should be empty');
    }
  }
  _retryAdd = (fn: (cancel: boolean) => void) => {
    this._retryOutbox.push(fn);
  }
  _retryRemove = (fn: (cancel: boolean) => void) => {
    this._retryOutbox = this._retryOutbox.filter(i => i !== fn);
  }
  wakeAll = () => {
    if (this._retryOutbox.length > 0) {
      this._retryOutbox.forEach(fn => fn(false));
      ASSERT(this._retryOutbox.length == 0, 'forceRetryAll - retryOutput should be empty');
    }
  }
}

const sharedRetryWaker = createShared<RetryWaker>('RetryWaker');

const _setWakeableTimeout = (fn: (shouldResetDuration: boolean) => void, msecs: number = 0, debugName: string = ''): NodeJS.Timer | number => {
  VERBOSE && console.log(`setForcibleTimeout ${msecs} [${debugName}]`);
  const current = sharedRetryWaker.value();
  if (current == undefined) {
    console.warn('invalid retry preemptor');
    return setTimeout(() => { }, 0);
  } else if (msecs <= 0) {
    return setTimeout(() => { try { fn(false) } catch (err) { console.error(err) } }, 0);
  } else if (msecs <= 1000) {
    return setTimeout(() => { try { fn(false) } catch (err) { console.error(err) } }, msecs);
  } else {
    const timer = setTimeout(() => {
      (current as RetryWaker)._retryRemove(wfn);
      _setWakeableTimeout(fn, msecs - 1000, debugName);
    }, 1000);
    const wfn = (cancel: boolean) => {
      clearTimeout(timer);
      (current as RetryWaker)._retryRemove(wfn);
      !cancel && _setWakeableTimeout(() => fn(true), 0, debugName);
    };
    (current as RetryWaker)._retryAdd(wfn);
    return timer;
  }
};

const _getNextTimeoutDuration = (durationIndex: number): number => {
  //const secsToWait = [1, 5, 15, 1 * 60, 3 * 60, 5 * 60, 10 * 60, 30 * 60, 1 * 60 * 60];
  const msecsToWait = [5, 15, 30, 60].map(i => i * 1000);
  return durationIndex < msecsToWait.length ? msecsToWait[durationIndex] : msecsToWait[msecsToWait.length - 1];
}

export class WakeableTimeout {
  private debugName: string;
  private durationIndex = 0;
  constructor(debugName: string = '') {
    this.debugName = debugName;
  }
  wake = (fn: () => void) => {
    const msecs = _getNextTimeoutDuration(this.durationIndex++);
    _setWakeableTimeout(shouldResetDuration => {
      if (shouldResetDuration) this.durationIndex = 0;
      fn();
    }, msecs, this.debugName);
  }
}

export const keepTrying = <R>(fn: () => Promise<R>, debugName: string): Promise<R> => {
  throw new Error('KEEP TRYING DISABLED FOR NOW');
  /*
  const recur = async (depth: number): Promise<R> => {
    if (depth > 0) console.log(`RETRY: ${debugName}`);
    const key = `${debugName}[${depth}]`;
    DEBUG_RETRY && _debugAdd(key);
    const ms = _getNextTimeoutDuration(depth);
    try {
      Root.dispatch(Root.actions().incrementRequestCount());
      const ret = await fn();
      Root.dispatch(Root.actions().decrementRequestCount());
      DEBUG_RETRY && _debugRem(key);
      return ret;
    } catch (err) {
      Root.dispatch(Root.actions().decrementRequestCount());
      DEBUG_RETRY && _debugRem(key);

      if (err && err.message && /Network request failed/.test(err.message)) {
        console.warn(`keepTryingWithDecay${debugName == undefined ? '' : ` (${debugName})`} [1] error(${ms / 1000}) code(${err.code}) status(${err.status}) - ${err && err.message || JSON.stringify(err)}`);
      } else if (err && err.code == 50200 || err.status >= 400) {
        console.error(`keepTryingWithDecay${debugName == undefined ? '' : ` (${debugName})`} [2 THROW] error(${ms / 1000}) code(${err.code}) status(${err.stauts}) - ${err && err.message || JSON.stringify(err)}`);
        throw err; // if user not found, bubble up exception
      } else {
        console.warn(`keepTryingWithDecay${debugName == undefined ? '' : ` (${debugName})`} [3] error(${ms / 1000}) code(${err.code}) status(${err.status}) - ${err && err.message || JSON.stringify(err)}`);
      }

      return await new Promise<R>((yes, no) => {
        try {
          _setWakeableTimeout(shouldResetDuration => {
            yes(recur(shouldResetDuration ? 0 : depth + 1));
          }, ms, debugName);
        } catch (err) {
          console.warn(`retry error - ${err && err.message || err}`);
          no(err);
        }
      });
    }
  };
  try {
    return recur(0);
  } catch (err) {
    return new Promise((yes, no) => {
      console.error(`RETRY ERROR DROPPED - ${err}`);
    });
  }
  */
}

export const useRetryWaker = () => {
  const ret = useShared(sharedRetryWaker, update => update(
    async () => new RetryWaker(),
    async p => p._shutdown()), []);
  return ret;
}