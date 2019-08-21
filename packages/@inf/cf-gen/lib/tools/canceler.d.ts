import { Result } from './result';
interface Canceler {
    add: <T, _ = never>(promise: Promise<T>) => Promise<T>;
    add_to: <T, _ = never>(promise: Promise<T>) => Promise<Result<T>>;
    cancel(): void;
}
declare class BasicCanceler implements Canceler {
    private _fns;
    _push: (owner: any, fn: () => void) => number;
    _remove: (owner: any) => [any, () => void][];
    add<T>(promise: Promise<T>): Promise<T>;
    add_to<T>(promise: Promise<T>): Promise<Result<T>>;
    cancel: () => void;
}
export declare const createCanceler: () => BasicCanceler;
export {};
//# sourceMappingURL=canceler.d.ts.map