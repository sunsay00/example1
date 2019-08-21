import { Kind } from 'graphql/language';
import { GraphQLScalarType } from 'graphql';
import { GraphQLError } from 'graphql/error';
import { Point } from '../types';

export class GraphQLRegExp extends GraphQLScalarType {
  constructor(opts: { name: string, exp: RegExp, err: string, description?: string }) {
    super({
      name: opts.name,
      description: opts.description,
      serialize: (value: string) => value,
      parseValue: (value: string) => {
        if (!opts.exp.test(value)) {
          throw new Error(opts.err);
        }
        return value;
      },
      parseLiteral: ast => {
        if (ast.kind !== Kind.STRING) {
          throw new GraphQLError(`Query error: expected string, got ${ast.kind}`, [ast]);
        }
        if (!opts.exp.test(ast.value)) {
          throw new GraphQLError(opts.err, [ast]);
        }
        return ast.value;
      }
    });
  }
}

export const GraphQLEmail = new GraphQLRegExp({
  name: 'Email',
  exp: /^(([^<>()[\]\.,;:\s@\"]+(\.[^<>()[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i,
  err: 'invalid email',
  description: 'an email type following RFC 5322'
});

export const GraphQLDateTime = new GraphQLScalarType({
  name: 'DateTime',
  description: 'a datetime type of format YYYY-MM-DDTHH:MM:SS.SSSZ',
  serialize: (val: Date | string) => {
    const value = typeof val == 'string' ? new Date(val) : val;
    if (!(value instanceof Date)) {
      throw new Error(`CustomType serialize error: expected an instance of Date, recevied ${JSON.stringify(value)} ${typeof value}`);
    }
    if (isNaN(value.getTime())) {
      throw new Error(`CustomType serialize error: invalid Date, recevied ${value}`);
    }
    return value.toJSON();
  },
  parseValue: (value: string) => {
    const date = new Date(value);
    if (isNaN(date.getTime())) {
      throw new Error('CustomType parseValue error: invalid Date');
    }
    return date;
  },
  parseLiteral: ast => {
    if (ast.kind !== Kind.STRING) {
      throw new GraphQLError(`CustomType parseLiteral error: expected a date string, got ${ast.kind}`, [ast]);
    }
    const result = new Date(ast.value);
    if (isNaN(result.getTime())) {
      throw new GraphQLError('CustomType parseLiteral error: invalid date', [ast])
    }
    if (ast.value !== result.toJSON()) {
      throw new GraphQLError('CustomType parseLiteral error: Invalid date format, only accepts: YYYY-MM-DDTHH:MM:SS.SSSZ', [ast])
    }
    return result;
  }
});

export const GraphQLPoint = new GraphQLScalarType({
  name: 'Point',
  description: 'a longitude latitude coordinate pair',
  serialize: (val: Point | string) => {
    if (typeof val == 'string') {
      try {
        return JSON.parse(val);
      } catch (err) {
        throw new Error('failed to parse json');
      }
    } else {
      return val;
    }
  },
  parseValue: (value: Point | string) => {
    try {
      const { lon, lat } = typeof value == 'string' ? JSON.parse(value) : value;
      if (typeof lon != 'number' || typeof lat != 'number') {
        throw new Error('CustomType parseValue error: invalid Point (1)');
      }
      const ret: Point = { lon, lat };
      return ret;
    } catch (err) {
      throw new Error(`CustomType parseValue error: invalid Point (2) ${err}`);
    }
  },
  parseLiteral: ast => {
    if (ast.kind !== Kind.STRING) {
      throw new GraphQLError(`CustomType parseLiteral error: expected a point string, got ${ast.kind}`, [ast]);
    }
    try {
      const { lon, lat } = JSON.parse(ast.value);
      if (typeof lon != 'number' || typeof lat != 'number') {
        throw new Error('CustomType parseLiteral value error: invalid Point (1)');
      }
      const ret: Point = { lon, lat };
      return ret;
    } catch (err) {
      throw new Error(`CustomType parseLiteral error: invalid Point (2) ${err}`);
    }
  }
});
