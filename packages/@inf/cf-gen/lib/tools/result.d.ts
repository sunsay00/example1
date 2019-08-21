export declare type Result<T> = {
    __result: boolean;
    err?: any;
    ok: T;
};
export declare const fn: <R>(f: () => void | Promise<R>) => Promise<Result<R>>;
export declare function to<T, U = never>(promise: Promise<T>): Promise<Result<T>>;
export declare const ok: <T, U = never>(v: T) => Result<T>;
export declare const er: <T, U = never>(v: any) => Result<T>;
//# sourceMappingURL=result.d.ts.map