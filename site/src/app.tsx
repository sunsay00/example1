import * as React from 'react';
import * as UI from '@inf/core-ui';
import * as Web from '@inf/gatsby-theme-web-ui';
import { AccountProvider, useAccount, AccountConsumer } from '@inf/cf-cognito';
import { ApolloProvider, ApolloResolvers } from '@inf/apollo';
import { Auth } from './components/auth';
import vars from './_vars';

const config = {
  WEBSOCKET_ENDPOINT: undefined,
  GRAPHQL_ENDPOINT: vars.GRAPHQL_ENDPOINT,
  AWS_REGION: vars.AWS_REGION,
  IDENTITY_POOL_ID: vars.IDENTITY_POOL_ID,
  USER_POOL_ID: vars.USER_POOL_ID,
  CLIENT_ID: vars.CLIENT_ID
};

const Layout = (props: { children?: React.ReactNode }) => {
  const { loading } = UI.useLoading(Layout);
  const { setCurrent } = Web.usePopUp();
  const { user, logOut } = useAccount();

  return (
    <Web.NavLayout renderNavBar={() =>
      <UI.ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32, paddingHorizontal: 32 }}>
        <Web.NavBar renderLogo={() => <UI.Image style={{ width: 150 }} resizeMode="contain" source={{ uri: 'logo.png' }} />}>
          <Web.NavLink to="#services">Services</Web.NavLink>
          <Web.NavLink to="#contact">Contact</Web.NavLink>
          <Web.NavLink to="/styleguide/">Styleguide</Web.NavLink>
          <Web.NavLink >Disabled</Web.NavLink>
          <Web.NavLink disabled={loading} onPress={() => {
            if (user) logOut();
            else setCurrent(<Auth onLogInComplete={() => setCurrent(null)} />);
          }}>
            {user ? 'Log out' : 'Log in'}
          </Web.NavLink>
        </Web.NavBar>
      </UI.ImageBackground>
    }>
      {props.children}
    </Web.NavLayout>
  );
}

import gql from 'graphql-tag';
const resolvers: ApolloResolvers = {
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
export const App = (props: { children?: React.ReactNode }) =>
  <Web.Root>{overlays =>
    <AccountProvider
      region={config.AWS_REGION}
      identityPoolId={config.IDENTITY_POOL_ID}
      userPoolId={config.USER_POOL_ID}
      clientId={config.CLIENT_ID}
    >
      <AccountConsumer>{account => {
        const authorization = account && account.user && `Bearer ${account.user.tokens.idToken}` || 'Guest';
        return <ApolloProvider
          disableCache
          authorization={authorization}
          websocketEndpoint={config.WEBSOCKET_ENDPOINT}
          graphqlEndpoint={config.GRAPHQL_ENDPOINT}
          resolvers={resolvers}
        >
          <UI.AssertSingleton fn={App}>
            <Layout>{props.children}</Layout>
            {overlays}
          </UI.AssertSingleton>
        </ApolloProvider>
      }}</AccountConsumer>
    </AccountProvider>
  }
  </Web.Root >