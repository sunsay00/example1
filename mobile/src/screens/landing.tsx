import * as React from 'react';
import * as UI from 'core-ui';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';
import { Home } from './home';
import { NavHeaderButton } from '../components/navheaderbutton';

export const Landing = (props: {}) => {
  const nav = useNavigation();
  useNavigationOptions({
    title: 'Home',
    headerRight: <NavHeaderButton onPress={() => nav.navigate('LogIn')}>Log in</NavHeaderButton>,
  });
  return (
    <Home />
  );
}
