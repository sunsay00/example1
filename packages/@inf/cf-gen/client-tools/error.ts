class _Err extends Error {
  constructor(m: string, ...prev: any[]) {
    super(prev != undefined ? `${m}\n${prev.join('\n')}` : m);
    console.error(m);
  }
}

export const ERROR = (...args: any[]): _Err => new _Err(args.join(' '));