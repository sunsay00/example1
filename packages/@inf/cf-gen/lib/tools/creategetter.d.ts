export declare type Getter<R> = {
    promise: () => Promise<R>;
    current: () => R | undefined;
};
export declare const createGetter: <R>(p: Promise<R>, onReady?: ((r: R) => void) | undefined) => Getter<R>;
//# sourceMappingURL=creategetter.d.ts.map