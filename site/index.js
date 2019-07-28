// @ts-check

import gql from 'graphql-tag';
import * as React from 'react';
import { AsyncStorage } from 'react-native';
import { ApolloProvider } from 'react-apollo-hooks';
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
import { library } from '@fortawesome/fontawesome-svg-core';
import { faGlobe, faServer, faBolt, faQuestion, faBars, faPlay, faPause, faTimesCircle, faUpload, faRedo, faVideo, faSync } from '@fortawesome/free-solid-svg-icons';

library.add(faGlobe);
library.add(faServer);
library.add(faBolt);
library.add(faQuestion);
library.add(faBars);
library.add(faPlay);
library.add(faPause);
library.add(faTimesCircle);
library.add(faUpload);
library.add(faRedo);
library.add(faVideo);
library.add(faSync);

const createClient = async () => {
  const cache = new InMemoryCache();
  await persistCache({ cache, storage: AsyncStorage });

  const httpLink = new HttpLink({ uri: process.env.GRAPHQL_ENDPOINT });

  /**
   * @type {WebSocketLink}
   */
  const wsLink = new WebSocketLink(new SubscriptionClient(
    process.env.WEBSOCKET_ENDPOINT,
    { reconnect: true, lazy: true },
    undefined,
    []
  ));
  // Workaround: https://github.com/apollographql/subscriptions-transport-ws/issues/377
  // @ts-ignore
  wsLink.subscriptionClient.maxConnectTimeGenerator.setMin(3000);
  // @ts-ignore
  wsLink.subscriptionClient.maxConnectTimeGenerator.duration = () => wsLink.subscriptionClient.maxConnectTimeGenerator.max;

  const authLink = setContext((_, { headers }) => {
    const token = localStorage.token;
    return {
      headers: {
        ...headers,
        authorization: token ? `Bearer ${token}` : ''
      }
    }
  });

  const client = new ApolloClient({
    link: ApolloLink.from([
      onError(({ graphQLErrors, networkError }) => {
        if (graphQLErrors)
          graphQLErrors.map(({ message, path }) =>
            console.error(`[GraphQL error]: Message: ${message}, Path: ${path}`));
        if (networkError)
          console.error(`[Network error]: ${networkError}`);
        debugger;
      }),
      split(
        ({ query }) => {
          const def = getMainDefinition(query);
          return def.kind === 'OperationDefinition' && def.operation === 'subscription';
        },
        wsLink,
        authLink.concat(httpLink),
      )
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
  });
  return client;
}

const Root = props => {
  const [client, setClient] = React.useState(null);

  React.useEffect(() => {
    console.log('create client');
    createClient().then(c => setClient(c)).catch(console.error);
  }, []);

  //const [authToken/*, refresh*/] = useAuthToken({ region: process.env.AWS_REGION, email: 'guest@guest.com', username: 'guest', password: 'guestguest' });
  //if (!authToken) return null;
  //localStorage.setItem('token', authToken);

  if (!client) return null;

  //client.resetStore();

  return (
    <ApolloProvider client={client}>
      {props.children}
    </ApolloProvider>
  );
}

export const wrapPageElement = ({ element }) => <Root>{element}</Root>
