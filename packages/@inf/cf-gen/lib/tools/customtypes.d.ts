import { GraphQLScalarType } from 'graphql';
export declare class GraphQLRegExp extends GraphQLScalarType {
    constructor(opts: {
        name: string;
        exp: RegExp;
        err: string;
        description?: string;
    });
}
export declare const GraphQLEmail: GraphQLRegExp;
export declare const GraphQLDateTime: GraphQLScalarType;
export declare const GraphQLPoint: GraphQLScalarType;
//# sourceMappingURL=customtypes.d.ts.map