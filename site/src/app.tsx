import * as React from 'react';
import * as UI from 'core-ui';
import * as Web from 'gatsby-theme-core-ui';
import { AccountProvider, useAccount } from 'cf-cognito';
import { ApolloProvider } from './hooks/useapollo';
import { Auth } from './components/auth';
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
  const { loading } = UI.useLoading(Layout);
  const { setCurrent } = Web.usePopUp();
  const { user, logOut } = useAccount();

  return (
    <Web.NavLayout renderNavBar={() =>
      <UI.ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32, paddingHorizontal: 32 }}>
        <Web.NavBar renderLogo={() => <UI.Image style={{ width: 150 }} resizeMode="contain" source={{ uri: 'logo.png' }} />}>
          <Web.NavLink to="/styleguide/">Styleguide</Web.NavLink>
          <Web.NavLink to="#services">Services</Web.NavLink>
          <Web.NavLink to="#contact">Contact</Web.NavLink>
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

export const App = (props: { children?: React.ReactNode }) =>
  <Web.Root>{overlays =>
    <AccountProvider region="us-east-1">
      <ApolloProvider>
        <Layout>
          {props.children}
          {overlays}
        </Layout>
      </ApolloProvider>
    </AccountProvider>}
  </Web.Root>