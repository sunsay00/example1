import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { AccountProvider, useAccount } from 'gatsby-theme-core-ui';
import { ApolloProvider } from './hooks/useapollo';
import { Authentication } from './components/authentication';
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

const Layout = (props: { children?: React.ReactNode }) => {
  const { setCurrent } = UI.usePopUp();
  const { loading, user, logOut } = useAccount();

  return (
    <UI.WebNavLayout renderNavBar={() =>
      <UI.ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32, paddingHorizontal: 32 }}>
        <UI.WebNavBar renderLogo={() => <UI.Image style={{ width: 150 }} resizeMode="contain" source={{ uri: 'logo.png' }} />}>
          <UI.WebNavLink to="/styleguide/">Styleguide</UI.WebNavLink>
          <UI.WebNavLink to="#services">Services</UI.WebNavLink>
          <UI.WebNavLink to="#contact">Contact</UI.WebNavLink>
          <UI.WebNavLink >Disabled</UI.WebNavLink>
          <UI.WebNavLink disabled={loading} onPress={() => {
            if (user) logOut();
            else setCurrent(<Authentication onLogInComplete={() => setCurrent(null)} />);
          }}>
            {user ? 'Log out' : 'Log in'}
          </UI.WebNavLink>
        </UI.WebNavBar>
      </UI.ImageBackground>
    }>
      {props.children}
    </UI.WebNavLayout>
  );
}

export const App = (props: { children?: React.ReactNode }) =>
  <UI.Injector>
    <UI.LoadingProvider>
      <AccountProvider region="us-east-1">
        <ApolloProvider>
          <UI.Root>
            <Layout>
              {props.children}
            </Layout>
          </UI.Root>
        </ApolloProvider>
      </AccountProvider>
    </UI.LoadingProvider>
  </UI.Injector>