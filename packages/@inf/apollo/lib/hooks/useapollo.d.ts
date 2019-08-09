import * as React from 'react';
import { ApolloClient } from 'apollo-client';
export declare const useApollo: () => ApolloClient<unknown>;
export declare const ApolloProvider: (props: {
    authorization: string;
    websocketEndpoint: string | undefined;
    graphqlEndpoint: string;
    children?: React.ReactNode;
}) => JSX.Element | null;
//# sourceMappingURL=useapollo.d.ts.map