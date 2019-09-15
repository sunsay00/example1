import { to } from './result';
import { ERROR } from './error';

type ShareState<R> = {
  getCount: number,
  refCount: number,
  cachedArgs?: unknown[],
  mode: 'UNDEFINED' | 'BUSY' | 'DEFINED',
  ctorOutbox: ((result: boolean) => void)[],
  result?: R,
  ctorFn?: () => Promise<R>
  dtorFn?: (r: R) => Promise<void>
};

const inputsAreEqual = (previnputs: unknown[] | undefined, nextinputs: unknown[] | undefined, debugString?: string) => {
  if (previnputs == undefined && nextinputs == undefined) return true;
  else if (previnputs == undefined || nextinputs == undefined || previnputs.length != nextinputs.length) return false;
  else {
    for (let i = 0; i < previnputs.length; ++i) {
      if (!Object.is(previnputs[i], nextinputs[i])) {
        console.log(`${debugString} shared argument at index[${i}] has changed, needs update`);
        return false;
      }
    }
    return true;
  }
}

export type Shared<R> = {
  value: () => R | undefined,
  update: (ctorFn: () => Promise<R>, dtorFn?: ((t: R) => Promise<void>), args?: unknown[]) => Promise<boolean>,
  addRef: () => void,
  remRef: () => Promise<number>,
}

export const createShared = <R>(name: string): Shared<R> => {
  let state = { getCount: 0, refCount: 0, mode: 'UNDEFINED', ctorOutbox: [], cachedArgs: undefined } as ShareState<R>;
  const update = (ctorFn: () => Promise<R>, dtorFn?: ((r: R) => Promise<void>), args?: unknown[]) => {
    state.ctorFn = ctorFn;
    state.dtorFn = dtorFn;
    ++state.getCount;
    if (state.mode == 'BUSY') {
      return new Promise<boolean>(async resolve => state.ctorOutbox.push(resolve));
    } else {
      const needsUpdate = state.refCount > 0 && (state.mode == 'UNDEFINED' || !inputsAreEqual(state.cachedArgs, args, name));
      if (needsUpdate) {
        state.mode = 'BUSY';
        return new Promise<boolean>(async (resolve, reject) => {
          try {
            const prevResult = state.result;
            if (dtorFn != undefined && prevResult != undefined) {
              console.log(`STOPPING: ${name}`);
              const ret = await to(dtorFn(prevResult));
              if (ret.err) console.error(ret.err);
              else console.log(`STOPPED: ${name}`);
              state.result = undefined;
            }

            console.log(`STARTING: ${name}`);
            const ret = await to(ctorFn());
            if (ret.err) {
              console.log(`START FAILED: ${name}`);
              state.cachedArgs = undefined;
              state.mode = 'UNDEFINED';
              state.ctorOutbox = [];
              state.result = undefined;
              console.error(ret.err);
              reject(ret.err);
            } else {
              console.log(`STARTED: ${name}`);
              const o = state.ctorOutbox;
              state.cachedArgs = args;
              state.mode = 'DEFINED';
              state.ctorOutbox = [];
              state.result = ret.ok;
              resolve(true);
              o.map(i => i(true));
            }
          } catch (err) {
            reject(err);
          }
        });
      } else {
        return Promise.resolve(false);
      }
    }
  };
  return {
    value: () => state.mode != 'DEFINED' ? undefined : state.result,
    addRef: () => {
      //console.log(`ADD-REF: ${name}`);
      ++state.refCount;
    },
    remRef: async () => {
      //console.log(`REM-REF ${name}`);
      if (state.refCount < 1)
        throw ERROR('bad releaseRef');
      --state.refCount;
      if (state.ctorFn && state.cachedArgs != undefined && state.refCount == 0 && state.getCount > 0) {
        await update(state.ctorFn, state.dtorFn, state.cachedArgs);
        const result = state.result;
        if (state.dtorFn != undefined && result != undefined) {
          state.getCount = 0;
          state.cachedArgs = undefined;
          state.mode = 'UNDEFINED';
          state.ctorOutbox = [];
          state.result = undefined;

          console.log(`STOPPING (2): ${name}`);
          const ret = await to(state.dtorFn(result));
          if (ret.err) console.error(ret.err);
          else console.log(`STOPPED (2): ${name}`);
        }
      }
      return state.refCount;
    },
    update: (ctorFn: () => Promise<R>, dtorFn?: ((t: R) => Promise<void>), args?: unknown[]): Promise<boolean> => {
      if (state.refCount <= 0) throw ERROR(`dead shared instance ${name}`);
      return update(ctorFn, dtorFn, args);
    },
  };
};