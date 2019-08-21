import { graphql, GraphQLSchema } from 'graphql';
import { makeExecutableSchema } from 'graphql-tools';
import typeDefs from '../back/api/src/api/resolver/v1/typedefs';
import resolvers from '../back/api/src/api/resolver/v1/resolvers';
import Mapper from '../back/api/src/api/mapper';
import { IUserContext, Variables } from '../types';
import { grant } from '../tools/grant';

class TestingService {
  @grant(['admins'])
  privateUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }
  @grant([])
  protectedUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }
  publicUpperCase(user: IUserContext, arg: string): string { return arg.toUpperCase(); }
}

export class Resolver<C extends IUserContext> {
  private _stage: string;
  private _mapper: Mapper<C>;
  private _schema: GraphQLSchema;

  constructor(stage: string, mapper: Mapper<C>) {
    this._stage = stage;
    this._mapper = mapper;
    this._schema = makeExecutableSchema({
      typeDefs,
      resolvers: resolvers(this._stage, this._mapper, new TestingService())
    });
  }

  resolve = async <C extends IUserContext>(headers: { [_: string]: string }, query: string, variables: Variables, user: C) => {
    return graphql(this._schema, query, undefined, user, variables);
  }
}