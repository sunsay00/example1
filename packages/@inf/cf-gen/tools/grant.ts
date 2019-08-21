import { IUserContext } from '../types';

export type Group = 'guests' | 'testers' | 'buyers' | 'sales' | 'dealership' | 'admins' | '__postconfirm__';

export const grant = (groups: Group[]) => {
  return (target: any, propertyKey: string, descriptor: TypedPropertyDescriptor<(...params: any[]) => any>) => {
    const method = descriptor.value;
    if (method === undefined) {
      throw new Error('this decorator only works on methods');
    }
    descriptor.value = function () {
      const $ctx: IUserContext = arguments[0];
      if ($ctx == undefined) {
        throw new Error('unable to grant permissions, invalid $ctx object detected. are you passing the $ctx as the first parameter to the grant method?');
      } else if (!$ctx || !$ctx.groups || $ctx.groups.length === undefined ||
        !groups.reduce((r, p) => r && !!$ctx && !!$ctx.groups && $ctx.groups.indexOf(p) !== -1, true)) {
        throw new Error(`unauthorized access - required: ${JSON.stringify(groups)}, available: ${JSON.stringify($ctx ? ($ctx.groups) : undefined)}`);
      }
      return method.apply(this, arguments);
    };
  }
};
