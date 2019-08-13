import * as React from 'react';
import { AsyncStorage } from 'core-ui';
import gql from 'graphql-tag';
import { ApolloProvider as Apollo } from 'react-apollo-hooks';
import { ApolloClient } from 'apollo-client';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { persistCache } from 'apollo-cache-persist';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { ApolloLink, split } from 'apollo-link';
import { onError } from 'apollo-link-error';
import { getMainDefinition } from 'apollo-utilities';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import { setContext } from 'apollo-link-context';

const createCache = async () => {
  const cache = new InMemoryCache();
  await persistCache({ cache, storage: AsyncStorage as any });
  return cache;
}

const concatWebSocket = (link: ApolloLink, websocketEndpoint: string | undefined) => {
  if (!websocketEndpoint) return link;

  const wsLink = new WebSocketLink(new SubscriptionClient(
    websocketEndpoint,
    { reconnect: true, lazy: true },
    undefined,
    []
  ));
  (wsLink as any).subscriptionClient.maxConnectTimeGenerator.setMin(3000);
  (wsLink as any).subscriptionClient.maxConnectTimeGenerator.duration = () => (wsLink as any).subscriptionClient.maxConnectTimeGenerator.max;

  return split(op => {
    const def = getMainDefinition(op.query);
    return def.kind === 'OperationDefinition' && def.operation === 'subscription';
  }, wsLink, link);
}

const useAuthLink = (authorization: string) => {

  const [authLink, setAuthLink] = React.useState(setContext((_, ctx) => ({ ...ctx, headers: { ...ctx.headers, authorization } })));

  React.useEffect(() => {
    setAuthLink(setContext((_, ctx) => ({ ...ctx, headers: { ...ctx.headers, authorization } })));
  }, [authorization]);

  return authLink;
}

const ApolloContext = React.createContext<ApolloClient<unknown> | undefined>(undefined);

export const useApollo = () => {
  const ctx = React.useContext(ApolloContext);
  if (!ctx) throw new Error('invalid apollo context');
  return ctx;
}

export const ApolloProvider = (props: {
  authorization: string,
  websocketEndpoint: string | undefined,
  graphqlEndpoint: string,
  children?: React.ReactNode
}) => {
  const [client, setClient] = React.useState<ApolloClient<unknown> | undefined>(undefined);

  const authLink = useAuthLink(props.authorization);

  React.useEffect(() => {
    createCache().then(cache => {
      const httpLink = new HttpLink({ uri: props.graphqlEndpoint });
      setClient(new ApolloClient({
        link: ApolloLink.from([
          onError(({ graphQLErrors, networkError }) => {
            if (graphQLErrors)
              graphQLErrors.map(({ message, path }) =>
                console.error(`[GraphQL error]: Message: ${message}, Path: ${path}`));
            if (networkError)
              console.error(`[Network error]: ${networkError}`);
            debugger;
          }),
          concatWebSocket(authLink.concat(httpLink), props.websocketEndpoint),
        ]),
        cache,
        resolvers: {
          Video: {
            logs: (video, _args, { cache, getCacheKey }) => {
              const id = getCacheKey({ __typename: 'Video', id: video.id });
              const fragment = gql`fragment video on Video { logs @client }`;
              const prev = cache.readFragment({ fragment, id });
              if (!prev) return [];
              return prev.logs;
            },
          }
        }
      }));
    }).catch(console.error);
  }, [authLink, props.graphqlEndpoint, props.websocketEndpoint]);

  if (!client) return null;

  return (
    <ApolloContext.Provider value={client}>
      <Apollo client={client}>{props.children}</Apollo>
    </ApolloContext.Provider>
  );
}