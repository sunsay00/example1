import * as React from 'react';
import * as UI from 'core-ui';
import { useNavigation } from '../hooks/usenavigation';

export const Landing = (props: {}) => {
  const nav = useNavigation();
  return (
    <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
      <UI.Header1>Infinage Labs</UI.Header1>
      <UI.Link size="md" onPress={() => nav.navigate('Auth')}>Proceed</UI.Link>
    </UI.View>
  )
}
