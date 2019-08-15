import * as React from 'react';
import * as UI from 'core-ui';
import { useNav, useNavOptions, NavHeaderButton } from '../hooks/usenav';

const Specimen = (props: { title: string, children?: React.ReactNode }) =>
  <>
    <UI.Header4>{props.title}</UI.Header4>
    <UI.View style={{ padding: 16, marginBottom: 8 }}>
      {props.children}
    </UI.View>
  </>

export const StyleGuide = () => {
  const toast = UI.useToast();
  const nav = useNav();
  useNavOptions({
    title: 'StyleGuide',
    headerLeft: <NavHeaderButton prefixIconName="menu" onPress={nav.openDrawer} />
  });
  return (
    <UI.ScrollView style={{ flex: 1, backgroundColor: UI.rgba(UI.Colors.black, 0.05) }}>
      <UI.View style={{ flex: 1, padding: 16, alignItems: 'stretch', justifyContent: 'center' }}>
        <UI.StatusBar barStyle='dark-content' />
        <UI.Header1>Infinage Styleguide</UI.Header1>
        <UI.Header2>Table of Contents</UI.Header2>
        <UI.Header2>Components</UI.Header2>
        <Specimen title="Accent">
          <UI.Accent secondary={false}>Accent</UI.Accent>
        </Specimen>
        <Specimen title="Badge">
          <UI.Badge value="13" />
        </Specimen>
        <Specimen title="BulleteCrumbs">
          <UI.BulletCrumbs crumbs={['one', 'two', ' three']} />
        </Specimen>
        <Specimen title="BulleteCrumbButtons">
          <UI.BulletCrumbButtons crumbs={['one', 'two', ' three']} />
        </Specimen>
        <Specimen title="BulleteCrumbButtons">
          <UI.Button onPress={() => UI.Alert.alert('button pressed!')}>Primary Button</UI.Button>
        </Specimen>
        {/*<Specimen title="Carousel">
          <UI.Carousel scrollEnabled infinite autoplay>
            <UI.View style={{
              height: 240, justifyContent: 'center',
              alignItems: 'center',
              backgroundColor: 'aliceblue'
            }}><UI.Text>Page 1</UI.Text></UI.View>
            <UI.View style={{
              height: 240, justifyContent: 'center',
              alignItems: 'center',
              backgroundColor: 'tomato'
            }}><UI.Text>Page 2</UI.Text></UI.View>
            <UI.View style={{
              height: 240, justifyContent: 'center',
              alignItems: 'center',
              backgroundColor: 'rebeccapurple',
            }}><UI.Text>Page 3</UI.Text></UI.View>
          </UI.Carousel>
        </Specimen>*/}
        <Specimen title="Icons">
          <UI.Icon name="planet" color="tomato" />
        </Specimen>
        <Specimen title="UserNameInput">
          <UI.UserNameInput placeholder="User Name" value="" />
        </Specimen>
        <Specimen title="PasswordInput">
          <UI.PasswordInput placeholder="Password" message="password required" value="" />
        </Specimen>
        <Specimen title="EmailInput">
          <UI.EmailInput placeholder="Email" disabled value="" />
        </Specimen>
        <Specimen title="NumericInput">
          <UI.EmailInput placeholder="Age" value="25" />
        </Specimen>
        <UI.Header1>Header 1</UI.Header1>
        <UI.Header2>Header 2</UI.Header2>
        <UI.Header3>Header 3</UI.Header3>
        <UI.Header4>Header 4</UI.Header4>
        <UI.Header5>Header 5</UI.Header5>
        <UI.Header6>Header 6</UI.Header6>
        <UI.Spacer />
        <UI.Button onPress={() => console.warn('!!!' + '123' + '!', 'abc')}>Log</UI.Button>
        <UI.Spacer />
        <UI.Button onPress={() => UI.Alert.alert('Title', 'Message', [
          { text: 'Option 1', onPress: () => toast.info('option 1 selected') },
          { text: 'Option 2', onPress: () => toast.info('option 2 selected') },
          {}])}>Alert</UI.Button>
      </UI.View>
    </UI.ScrollView>
  );
}
