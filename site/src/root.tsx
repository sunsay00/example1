import * as React from 'react';
import { AsyncStorage } from 'react-native';
import gql from 'graphql-tag';
import * as UI from 'gatsby-theme-core-ui';
import { BreakableProvider } from 'gatsby-theme-core-ui';
import { AccountProvider } from './hooks/useaccount';
import { useModal, ModalProvider } from './hooks/usemodal';
import { Authentication } from './components/authentication';
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
import { faChevronLeft, faTimes, faGlobe, faServer, faBolt, faQuestion, faBars, faPlay, faPause, faTimesCircle, faUpload, faRedo, faVideo, faSync } from '@fortawesome/free-solid-svg-icons';

library.add(faChevronLeft);
library.add(faTimes);
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

const createCache = async () => {
  const cache = new InMemoryCache();
  await persistCache({ cache, storage: AsyncStorage as any });
  return cache;
}

const httpLink = new HttpLink({ uri: process.env.GRAPHQL_ENDPOINT });

const authLink = setContext((_, { headers }) => {
  const token = localStorage.token;
  return {
    headers: {
      ...headers,
      authorization: token ? `Bearer ${token}` : ''
    }
  }
});

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

const useApolloClient = () => {
  const [client, setClient] = React.useState<ApolloClient<unknown> | undefined>(undefined);

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
  }, []);

  return client;
}

/*
import { Account } from 'cf-cognito';
 
const account = new Account('us-east-1', 'Web', AsyncStorage);
 
const main = async () => {
  await account.init();
  console.log('ready');
}
 
main().catch(err => console.error(err.message || err));
*/

const Application = (props: { children?: React.ReactNode }) => {
  const { setCurrent } = useModal();
  //const [authToken/*, refresh*/] = useAuthToken({ region: process.env.AWS_REGION, email: 'guest@guest.com', username: 'guest', password: 'guestguest' });
  //if (!authToken) return null;
  //localStorage.setItem('token', authToken);

  return (
    <UI.WebNavLayout renderNavBar={() =>
      <UI.ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32, paddingHorizontal: 32 }}>
        <UI.WebNavBar renderLogo={() => <UI.Image style={{ width: 150 }} resizeMode="contain" source={{ uri: 'logo.png' }} />}>
          <UI.WebNavLink to="/styleguide/">Styleguide</UI.WebNavLink>
          <UI.WebNavLink to="#services">Services</UI.WebNavLink>
          <UI.WebNavLink to="#contact">Contact</UI.WebNavLink>
          <UI.WebNavLink onPress={() => {
            console.log('!');
            setCurrent(<Authentication onLogInComplete={() => { }} />);
          }}>Log in</UI.WebNavLink>
          <UI.WebNavLink>Disabled</UI.WebNavLink>
        </UI.WebNavBar>
      </UI.ImageBackground>
    }>{props.children}</UI.WebNavLayout>
  );
}

export const Root = (props: { children?: React.ReactNode }) => {
  const client = useApolloClient();
  if (!client) return null;
  //client.resetStore();
  return (
    <ApolloProvider client={client}>
      <AccountProvider region="us-east-1">
        <BreakableProvider>
          <ModalProvider style={{ maxWidth: 500 }}>
            <Application>{props.children}</Application>
          </ModalProvider>
        </BreakableProvider>
      </AccountProvider>
    </ApolloProvider >
  );
}