import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { AccountProvider } from './hooks/useaccount';
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

const Application = (props: { children?: React.ReactNode }) => {
  const { setCurrent } = UI.useModal();

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
          <UI.WebNavLink onPress={() => setCurrent(<Authentication onLogInComplete={() => { }} />)}>Log in</UI.WebNavLink>
          <UI.WebNavLink >Disabled</UI.WebNavLink>
        </UI.WebNavBar>
      </UI.ImageBackground>
    }>{props.children}</UI.WebNavLayout>
  );
}

export const Root = (props: { children?: React.ReactNode }) =>
  <ApolloProvider>
    <AccountProvider region="us-east-1">
      <UI.WebRoot>
        <Application>{props.children}</Application>
      </UI.WebRoot>
    </AccountProvider>
  </ApolloProvider>