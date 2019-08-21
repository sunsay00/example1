import * as domain from 'domain';

/**
 * Decorates a lambda handle to run inside a domain, enabling
 * call stacks to be preserved bewteen asynchronous calls
 * @param {LambdaEvent} event - The event object passed by aws
 * @param {Hash<string>} context - The context object passed by aws
 * @returns a function wrapped inside a domain
 */
export const domainWrapper = <E, C, R>(fn: (event: E, context: C) => Promise<R>) => (event: E, context: C, next: any): void => {
  const d = domain.create();
  d.on('error', err => {
    try {
      console.error('DOMAIN ERROR:', err.message, err.stack);

      // Note: we're in dangerous territory!
      // By definition, something unexpected occurred,
      // which we probably didn't want.
      // Anything can happen now!  Be very careful!

      // Make sure we close down within 3 seconds
      const killtimer = setTimeout(() => {
        process.exit(1);
      }, 3000);
      // But don't keep the process open just for that!
      killtimer.unref();
      //next(null, { statusCode: 200, body: 'Oops, there was a problem' });
    } catch (err) {
      // oh well, not much we can do at this point.
      console.error('ERROR: could not send 500!', err.stack);
    }
  });
  d.add(event as any);
  d.add(context as any);
  d.add(next);
  d.run(() => {
    fn(event, context)
      .then(ret => next(null, ret))
      .catch(err => {
        console.error('DOMAIN ERROR (2):', err);
        next(err);
      });
  });
};

