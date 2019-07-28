import * as React from 'react';
import { View, Section, Icon, Breakable, sg, Animated, Easing, TouchableOpacity, Text, ImageBackground } from 'core-ui';

const WebSticky = (props: { offsetY: number, children?: React.ReactNode }) => {
  const navRef = React.useRef<HTMLElement | null>(null);
  React.useEffect(() => {
    navRef.current = document.querySelector('nav');
  }, []);
  return (
    <>
      <div style={{ height: props.offsetY }} />
      <nav style={{
        flex: 1, position: 'fixed',
        top: navRef.current && navRef.current.offsetTop || 0, left: 0, right: 0,
        backgroundColor: sg.colors.white, zIndex: 1,
      }}>
        {props.children}
      </nav>
    </>
  );
}

const NavLayoutContext = React.createContext({ height: 0 });

const NavContext = React.createContext((_opened: boolean) => { });

export const WebNavBar = (props: {
  renderLogo?: () => React.ReactNode,
  children?: React.ReactNode
}) => {
  const numChildren = React.Children.count(props.children);
  const [opened, setOpened] = React.useState(false);
  const animHeight = React.useRef(new Animated.Value(0)).current;
  const toggleHeight = React.useCallback((opened: boolean) => {
    const height = numChildren == 0 ? 0 : numChildren * 52 + (numChildren - 1);
    Animated.timing(animHeight, { toValue: opened ? height : 0, duration: 200, easing: Easing.quad }).start(() => setOpened(v => !v));
  }, [numChildren]);

  return (
    <NavLayoutContext.Consumer>{({ height }) =>
      <NavContext.Provider value={toggleHeight}>
        <Breakable renderSmall={() =>
          <>
            <Animated.View style={{ height: animHeight }}>
              {React.Children.map(props.children, child =>
                <View style={{
                  alignItems: 'center',
                  borderBottomColor: sg.rgba(sg.colors.white, .2),
                  borderBottomWidth: 1,
                  marginHorizontal: -32
                }}>
                  {React.isValidElement(child) ? React.cloneElement(child, {
                    ...child.props,
                    secondary: true
                  }) : child}
                </View>
              )}
            </Animated.View>
            <View style={{ flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', height, backgroundColor: sg.colors.white, marginHorizontal: -32, paddingHorizontal: 16 }}>
              <TouchableOpacity onPress={() => {
                const body = document.querySelector('body');
                body && body.scrollIntoView({ behavior: 'smooth' });
                toggleHeight(false);
              }}>
                {props.renderLogo && props.renderLogo()}
              </TouchableOpacity>
              {numChildren > 0 && <Icon size="sm" name="bars" onPress={() => toggleHeight(!opened)} />}
            </View>
          </>}
          renderMedium={() =>
            <WebSticky offsetY={height}>
              <Section>
                <View style={{ flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', height, marginHorizontal: 16 }}>
                  <TouchableOpacity onPress={() => {
                    const body = document.querySelector('body');
                    body && body.scrollIntoView({ behavior: 'smooth' });
                  }}>
                    {props.renderLogo && props.renderLogo()}
                  </TouchableOpacity>
                  <View style={{ flexDirection: 'row' }}>
                    {props.children}
                  </View>
                </View>
              </Section>
            </WebSticky>
          } />
      </NavContext.Provider>}
    </NavLayoutContext.Consumer>
  );
}

export const WebNavLink = (props: { secondary?: boolean, to: string, children?: React.ReactNode }) =>
  <NavContext.Consumer>{setOpened =>
    <TouchableOpacity onPress={() => {
      const anchor = document.querySelector(`#${props.to}`);
      if (anchor) {
        setOpened(false);
        anchor.scrollIntoView({ behavior: 'smooth' });
      }
    }}>
      <Text style={{
        padding: { xs: 4, sm: 8, md: 16 }.md,
        fontFamily: sg.fonts.sansSerif.weightProps.bold.name,
        fontWeight: sg.fonts.sansSerif.weightProps.bold.value,
        fontSize: sg.fonts.sansSerif.size.heading4,
        color: props.secondary ? sg.colors.white : sg.colors.black,
      }}>{props.children}</Text>
    </TouchableOpacity>
  }</NavContext.Consumer>

export const WebNavAnchor = (props: { id: string }) =>
  <NavLayoutContext.Consumer>{({ height }) =>
    <div id={`${props.id}`} style={{ top: -height, position: 'relative' }} />}
  </NavLayoutContext.Consumer>

export const WebNavLayout = (props: { navHeight?: number, children?: React.ReactNode, renderNavBar?: () => React.ReactNode }) => {
  const navHeight = props.navHeight || 80;
  return (
    <NavLayoutContext.Provider value={{ height: navHeight }}>
      <View style={{ minWidth: 300, marginHorizontal: 32, flex: 1, minHeight: '100vh' }}>
        <Breakable
          renderSmall={children => <>
            <ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32, paddingHorizontal: 32 }}>
              {props.renderNavBar && props.renderNavBar()}
            </ImageBackground>
            {children}
          </>}
          renderMedium={children => <>
            <View style={{ marginTop: navHeight, marginBottom: -navHeight }}>
              {children}
            </View>
            {props.renderNavBar && props.renderNavBar()}
          </>}>
          <View style={{ backgroundColor: sg.rgba(sg.colors.black, .03), marginHorizontal: -32, paddingHorizontal: 32, flexGrow: 1 }}>
            {props.children}
          </View>
        </Breakable>
      </View>
    </NavLayoutContext.Provider>
  );
}