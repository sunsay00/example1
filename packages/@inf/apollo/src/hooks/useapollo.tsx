import * as React from 'react';
import { AsyncStorage } from '@inf/core-ui';
import { ApolloProvider as Apollo, useApolloClient } from 'react-apollo-hooks';
import { ApolloClient, Resolvers } from 'apollo-client';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { persistCache } from 'apollo-cache-persist';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { ApolloLink, split } from 'apollo-link';
import { onError } from 'apollo-link-error';
import { getMainDefinition } from 'apollo-utilities';
import { SubscriptionClient } from 'subscriptions-transport-ws';
import { setContext } from 'apollo-link-context';
import * as UI from '@inf/core-ui';

export type ApolloResolvers = Resolvers;

const createCache = async (disableCache: boolean) => {
  const cache = new InMemoryCache();
  if (!disableCache)
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

const ApolloContext = React.createContext<() => (ApolloClient<object> | undefined)>(() => undefined);

export const ApolloConsumer = ApolloContext.Consumer;

export const useApollo = () => {
  const client = useApolloClient();
  if (!client) throw new Error('invalid apollo context');
  return client;
}

export const ApolloProvider = (props: {
  disableCache?: boolean,
  authorization: string,
  websocketEndpoint: string | undefined,
  graphqlEndpoint: string,
  children?: React.ReactNode,
  resolvers?: ApolloResolvers | ApolloResolvers[],
}) => {
  const [client, setClient] = React.useState<ApolloClient<object> | undefined>(undefined);
  const busyRef = React.useRef(false);

  React.useEffect(() => {
    if (busyRef.current) return;
    busyRef.current = true;
    createCache(props.disableCache || true).then(cache => {
      const httpLink = new HttpLink({ uri: props.graphqlEndpoint });
      const authLink = setContext((_, ctx) => ({ ...ctx, headers: { ...ctx.headers, authorization: props.authorization } }));
      setClient(new ApolloClient({
        link: ApolloLink.from([
          onError(({ graphQLErrors, networkError }) => {
            if (graphQLErrors)
              graphQLErrors.map(({ message, path }) =>
                console.error(`[GraphQL error]: Message: ${message}, Path: ${path}`));
            if (networkError)
              console.error(`[Network error]: ${networkError} ${props.graphqlEndpoint}`);
            //debugger;
          }),
          concatWebSocket(authLink.concat(httpLink), props.websocketEndpoint),
        ]),
        cache,
        resolvers: props.resolvers
      }));
    }).catch(console.error).finally(() => busyRef.current = false);
  }, [props.authorization, props.graphqlEndpoint, props.websocketEndpoint, props.resolvers]);

  if (UI.useSSR()) return null;
  if (!client) return null;

  return (
    <Apollo client={client}>{props.children}</Apollo>
  );
}