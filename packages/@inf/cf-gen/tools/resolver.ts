import { graphql, GraphQLSchema } from 'graphql';
import { makeExecutableSchema } from 'graphql-tools';
import typeDefs from '../back/api/src/api/resolver/v1/typedefs';
import resolvers from '../back/api/src/api/resolver/v1/resolvers';
import Mapper from '../back/api/src/api/mapper';
import { IUserContext, Variables, ITestingService } from '../types';
import { grant } from '../tools/grant';

class TestingService implements ITestingService<IUserContext> {

  @grant(['admins'])
  async adminAuthorized(ctx: IUserContext, fields: string[], mapper: Mapper<IUserContext>, arg: string) {
    return arg.toUpperCase();
  }

  @grant([])
  async authorized(ctx: IUserContext, fields: string[], mapper: Mapper<IUserContext>, arg: string) {
    return arg.toUpperCase();
  }

  async unauthorized(ctx: IUserContext, fields: string[], mapper: Mapper<IUserContext>, arg: string) {
    const users = await mapper.usersFindAll_v1(ctx, fields);
    console.log('users', JSON.stringify(users));
    return arg.toUpperCase();
  }
};


export class Resolver<C extends IUserContext> {
  private _stage: string;
  private _mapper: Mapper<C>;
  private _schema: GraphQLSchema;
  private _onPre?: () => Promise<void>;
  private _onPost?: () => Promise<void>;

  constructor(stage: string, mapper: Mapper<C>, onPreResolve?: () => Promise<void>, onPostResolve?: () => Promise<void>) {
    this._stage = stage;
    this._mapper = mapper;
    this._schema = makeExecutableSchema({
      typeDefs,
      resolvers: resolvers(this._stage, this._mapper, new TestingService())
    });
    this._onPre = onPreResolve;
    this._onPost = onPostResolve;
  }

  resolve = async <C extends IUserContext>(headers: { [_: string]: string }, query: string, variables: Variables, user: C) => {
    this._onPre && await this._onPre();
    const ret = await graphql(this._schema, query, undefined, user, variables);
    this._onPost && await this._onPost();
    return ret;
  }
}