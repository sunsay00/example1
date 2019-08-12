import * as React from 'react';
import * as UI from 'core-ui';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';
import { NavHeaderButton } from '../components/navheaderbutton';

const Specimen = (props: { title: string, children?: React.ReactNode }) =>
  <>
    <UI.Header4>{props.title}</UI.Header4>
    <UI.View style={{ padding: 16, marginBottom: 8 }}>
      {props.children}
    </UI.View>
  </>

export const Home = () => {
  const toast = UI.useToast();
  const nav = useNavigation();
  useNavigationOptions({
    title: 'Home',
    headerLeft: <NavHeaderButton prefixIconName="menu" onPress={nav.openDrawer} />
  });
  return (
    <UI.ScrollView style={{ flex: 1 }}>
      <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center', padding: 16 }}>
        <UI.StatusBar barStyle='dark-content' />
        <UI.Header1>Business + Technology = Success</UI.Header1>
        <UI.Header4 serifed>Advocating for businesses since 2000</UI.Header4>
      </UI.View>

      <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center', backgroundColor: UI.rgba(UI.Colors.black, .02), padding: 16 }}>
        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="desktop" color={UI.Colors.accentBlue} />
          <UI.Header4 serifed>Experienced IT</UI.Header4>
          <UI.Text>Infinage specializes in project management, software development, network infrastructure design and implementation, IT security assessment, and IT consulting services.</UI.Text>
        </UI.View>

        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="flash" color={UI.Colors.accentGreen} />
          <UI.Header4 serifed>Efficiency driven</UI.Header4>
          <UI.Text>Our highly trained staffs provide end-to-end IT solutions to our clients from a variety of industries based on their specific needs.</UI.Text>
        </UI.View>

        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="globe" color={UI.Colors.accentRed} />
          <UI.Header4 serifed>Global solutions</UI.Header4>
          <UI.Text>We are steadily growing and evolving into a global IT Solutions firm as we continue to fulfill our clients’ escalating exigencies in the international markets.</UI.Text>
        </UI.View>

      </UI.View>
    </UI.ScrollView>
  );
}
