import * as React from 'react';
import { ApolloClient } from 'apollo-client';
import * as UI from '@inf/core-ui';
import { QueryResult } from '@apollo/react-common';

import { useApollo } from './hooks/useapollo';

type Paginated<T> = {
  cursor?: string,
  items: T[],
};

export const Loader = <Vars, Item>(props: {
  variables: Vars
  hooks: (client: ApolloClient<object>, variables: Vars) => QueryResult<Paginated<Item>>
  children: (items: Item[], opts: {
    fetching: boolean,
    refetch: () => Promise<void>,
    fetchMore?: () => void
  }) => JSX.Element
}) => {
  const [fetching, setFetching] = React.useState(false);
  const cursorRef = React.useRef<string | undefined>(undefined);
  const client = useApollo();
  const result = props.hooks(client, props.variables);
  if (!result.data) {
    return <UI.Loading />;
  } else if (result.error != undefined) {
    return <UI.TryAgain error={result.error} onRetry={async () => { await result.refetch(props.variables) }} />;
  } else {
    cursorRef.current = result.data.cursor || undefined;
    return props.children(result.data.items, {
      fetching,
      refetch: async () => { await result.refetch(props.variables); },
      fetchMore: !cursorRef.current ? undefined : () => {
        setFetching(true);
        result.fetchMore({
          variables: { ...props.variables, after: cursorRef.current },
          updateQuery: (prev, { fetchMoreResult }) => {
            setFetching(false);
            if (!fetchMoreResult) return prev;
            cursorRef.current = fetchMoreResult.cursor;
            return fetchMoreResult;
          },
        });
      }
    });
  }
}