import { Result } from './result';

export const ASSERT = <T>(c: Result<T> | boolean, ...msgs: any[]): void => {
  if (typeof c == 'boolean') {
    if (!c) {
      console.error(`ASSERTION-FAILED: ${msgs.join(' - ')}`);
      //Sentry.nativeCrash();
      throw new Error(`ASSERTION-FAILED: ${msgs.join(' - ')}`);
    }
  } else {
    if (c.err) {
      console.error(`ASSERTION-FAILED: ${c.err} - ${msgs.join(' - ')}`);
      //Sentry.nativeCrash();
      throw new Error(`ASSERTION-FAILED: ${c.err} - ${msgs.join(' - ')}`);
    }
  }
}