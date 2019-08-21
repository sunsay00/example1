import Mapper from '../back/api/src/api/mapper';
import { IUserContext, Variables } from '../types';
export declare class Resolver<C extends IUserContext> {
    private _stage;
    private _mapper;
    private _schema;
    constructor(stage: string, mapper: Mapper<C>);
    resolve: <C_1 extends IUserContext>(headers: {
        [_: string]: string;
    }, query: string, variables: Variables, user: C_1) => Promise<import("graphql").ExecutionResult<import("graphql/execution/execute").ExecutionResultDataDefault>>;
}
//# sourceMappingURL=resolver.d.ts.map