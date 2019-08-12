import * as React from 'react';
import * as UI from 'core-ui';
import { useAccount } from 'cf-cognito';
import { useNavigation, useNavigationOptions } from '../hooks/usenavigation';
import { NavHeaderButton } from '../components/navheaderbutton';

export const Home = () => {
  const { user } = useAccount();
  const nav = useNavigation();
  useNavigationOptions({
    title: 'Home',
    headerLeft: <NavHeaderButton prefixIconName="menu" onPress={nav.openDrawer} />
  });
  return (
    <UI.ScrollView style={{ flex: 1 }}>
      <UI.ImageBackground source={require('../../assets/images/image1.jpg')} style={{ width: '100%' }} resizeMode="cover">
        <UI.SafeAreaView style={{ backgroundColor: UI.rgba(UI.Colors.blue, .2) }}>
          <UI.View style={{
            flex: 1, alignItems: 'center', justifyContent: 'center',
            paddingHorizontal: 16,
            paddingVertical: 56,
          }}>
            <UI.StatusBar barStyle='dark-content' />
            <UI.Headline secondary>Business + Technology = Success</UI.Headline>
            <UI.Header4 secondary serifed>Advocating for businesses since 2000</UI.Header4>
            {!user && <UI.Button onPress={() => nav.navigate('LogIn')}>Log in</UI.Button>}
          </UI.View>
        </UI.SafeAreaView>
      </UI.ImageBackground>

      <UI.View style={{ flex: 1, alignItems: 'center', justifyContent: 'center', backgroundColor: UI.rgba(UI.Colors.black, .02), padding: 16 }}>
        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="desktop" color={UI.Colors.accentBlue} />
          <UI.Header3 serifed>Experienced IT</UI.Header3>
          <UI.Text>Infinage specializes in project management, software development, network infrastructure design and implementation, IT security assessment, and IT consulting services.</UI.Text>
        </UI.View>

        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="flash" color={UI.Colors.accentGreen} />
          <UI.Header3 serifed>Efficiency driven</UI.Header3>
          <UI.Text>Our highly trained staffs provide end-to-end IT solutions to our clients from a variety of industries based on their specific needs.</UI.Text>
        </UI.View>

        <UI.View style={{ alignItems: 'center', padding: 16 }}>
          <UI.Icon name="globe" color={UI.Colors.accentRed} />
          <UI.Header3 serifed>Global solutions</UI.Header3>
          <UI.Text>We are steadily growing and evolving into a global IT Solutions firm as we continue to fulfill our clients’ escalating exigencies in the international markets.</UI.Text>
        </UI.View>

      </UI.View>
    </UI.ScrollView>
  );
}