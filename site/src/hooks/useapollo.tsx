import * as React from 'react';
import { AsyncStorage } from 'react-native';
import gql from 'graphql-tag';
import * as UI from 'gatsby-theme-core-ui';
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
import { useAccount } from 'cf-cognito';

const createCache = async () => {
  const cache = new InMemoryCache();
  await persistCache({ cache, storage: AsyncStorage as any });
  return cache;
}

const httpLink = new HttpLink({ uri: process.env.GRAPHQL_ENDPOINT });

const concatWebSocket = (link: ApolloLink) => {
  if (!process.env.WEBSOCKET_ENDPOINT) return link;

  const wsLink = new WebSocketLink(new SubscriptionClient(
    process.env.WEBSOCKET_ENDPOINT,
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

const useAuthorizationHeader = () => {
  const { user } = useAccount();
  const [authorization, setAuthorization] = React.useState('Guest');
  React.useEffect(() => {
    if (user) setAuthorization(`Bearer ${user.tokens.idToken}`);
    else setAuthorization('Guest');
  }, [user]);
  return authorization;
}

const useAuthLink = () => {
  const authorization = useAuthorizationHeader();

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

export const ApolloProvider = (props: { children?: React.ReactNode }) => {
  const [client, setClient] = React.useState<ApolloClient<unknown> | undefined>(undefined);

  const authLink = useAuthLink();

  React.useEffect(() => {
    createCache().then(cache => {
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
          concatWebSocket(authLink.concat(httpLink)),
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
  }, [authLink]);

  if (!client) return <UI.Loading />;

  return (
    <ApolloContext.Provider value={client}>
      <Apollo client={client}>{props.children}</Apollo>
    </ApolloContext.Provider>
  );
}