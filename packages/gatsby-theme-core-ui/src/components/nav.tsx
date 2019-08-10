import * as React from 'react';
import { Colors, rgba, Fonts, View, Section, Icon, Breakable, Animated, Easing, TouchableOpacity, Text } from 'core-ui';
import { Link, navigate } from 'gatsby';

const Sticky = (props: { offsetY: number, children?: React.ReactNode }) => {
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
        backgroundColor: Colors.white, zIndex: 1,
      }}>
        {props.children}
      </nav>
    </>
  );
}

const NavLayoutContext = React.createContext({
  animHeight: new Animated.Value(0),
  height: 0,
  setOpened: (opened: boolean) => console.error('invalid navlayout context'),
  opened: false,
  setNumChildren: (num: number) => console.error('invalid navlayout context'),
  navHeight: (opened: boolean) => 0 as number,
});

export const NavBar = (props: {
  renderLogo?: () => React.ReactNode,
  children?: React.ReactNode
}) => {
  const numChildren = React.Children.count(props.children);
  const { setNumChildren, opened, setOpened, height, animHeight } = React.useContext(NavLayoutContext);

  React.useEffect(() => {
    setNumChildren(numChildren);
  }, [numChildren]);

  const goHome = () => {
    const rel = document.location.href.replace(/^(?:\/\/|[^\/]+)*\//, '');
    const splits = rel.split('/');
    if (splits.length > 1) {
      navigate('/');
    } else {
      const body = document.querySelector('body');
      body && body.scrollIntoView({ behavior: 'smooth' });
    }
  }

  return (
    <Breakable renderSmall={() =>
      <>
        <Animated.View style={{ height: animHeight }}>
          {React.Children.map(props.children, child =>
            <View style={{
              alignItems: 'center',
              borderBottomColor: rgba(Colors.white, .2),
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
        <View style={{ flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', height, backgroundColor: Colors.white, marginHorizontal: -32, paddingHorizontal: 16 }}>
          <TouchableOpacity onPress={() => {
            setOpened(false);
            goHome();
          }}>
            {props.renderLogo && props.renderLogo()}
          </TouchableOpacity>
          {numChildren > 0 && <Icon size="sm" name="menu" onPress={() => setOpened(!opened)} />}
        </View>
      </>}
      renderMedium={() =>
        <Sticky offsetY={0}>
          <Section>
            <View style={{ flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center', height, marginHorizontal: 16 }}>
              <TouchableOpacity onPress={goHome}>
                {props.renderLogo && props.renderLogo()}
              </TouchableOpacity>
              <View style={{ flexDirection: 'row' }}>
                {props.children}
              </View>
            </View>
          </Section>
        </Sticky>
      } />
  );
}

export const NavLink = (props: {
  secondary?: boolean,
  to?: string,
  onPress?: () => void,
  children?: React.ReactNode,
  disabled?: boolean
}) => {
  const { setOpened } = React.useContext(NavLayoutContext);
  if (props.onPress) {
    return (
      <TouchableOpacity disabled={props.disabled} onPress={() => {
        setOpened(false);
        props.onPress && props.onPress();
      }}>
        <Text style={{
          padding: { xs: 4, sm: 8, md: 16 }.md,
          fontFamily: Fonts.sansSerif.weightProps.bold.name,
          fontWeight: Fonts.sansSerif.weightProps.bold.value,
          fontSize: Fonts.sansSerif.size.heading4,
          color: props.secondary ? Colors.white : Colors.black,
        }}>{props.children}</Text>
      </TouchableOpacity>
    );
  }

  if (props.to) {
    const to = props.to;
    const rel = document.location.href.replace(/^(?:\/\/|[^\/]+)*/, '');
    if (props.to.startsWith('#') && rel.startsWith('/#')) {
      return (
        <TouchableOpacity disabled={props.disabled} onPress={() => {
          const anchor = document.querySelector(to);
          if (anchor) {
            setOpened(false);
            anchor.scrollIntoView({ behavior: 'smooth' });
          }
        }}>
          <Text style={{
            padding: { xs: 4, sm: 8, md: 16 }.md,
            fontFamily: Fonts.sansSerif.weightProps.bold.name,
            fontWeight: Fonts.sansSerif.weightProps.bold.value,
            fontSize: Fonts.sansSerif.size.heading4,
            color: props.secondary ? Colors.white : Colors.black,
          }}>{props.children}</Text>
        </TouchableOpacity>
      );
    } else {
      return (
        <Link to={props.disabled ? '' : to.startsWith('/') ? to : `/${to}`} style={{ textDecoration: 'none' }} onMouseUp={() => setOpened(false)}>
          <Text style={{
            padding: { xs: 4, sm: 8, md: 16 }.md,
            fontFamily: Fonts.sansSerif.weightProps.bold.name,
            fontWeight: Fonts.sansSerif.weightProps.bold.value,
            fontSize: Fonts.sansSerif.size.heading4,
            color: props.secondary ? Colors.white : Colors.black,
          }}>{props.children}</Text>
        </Link>
      );
    }
  }

  return (
    <Text style={{
      padding: { xs: 4, sm: 8, md: 16 }.md,
      fontFamily: Fonts.sansSerif.weightProps.bold.name,
      fontWeight: Fonts.sansSerif.weightProps.bold.value,
      fontSize: Fonts.sansSerif.size.heading4,
      color: rgba(props.secondary ? Colors.white : Colors.black, .5)
    }}>{props.children}</Text>
  );
}

export const NavAnchor = (props: { id: string }) => {
  const { opened, height, navHeight } = React.useContext(NavLayoutContext);
  return (
    <div id={`${props.id}`} style={{ top: opened ? -navHeight(opened) : -height, position: 'relative' }} />
  );
}

export const NavLayout = (props: { navHeight?: number, children?: React.ReactNode, renderNavBar?: () => React.ReactNode }) => {
  const height = props.navHeight || 80;
  const [opened, _setOpened] = React.useState(false);
  const [numChildren, setNumChildren] = React.useState(0);
  const animHeight = React.useRef(new Animated.Value(0)).current;
  const navHeight = (opened: boolean) => opened ? numChildren == 0 ? 0 : numChildren * 52 + (numChildren - 1) : 0;
  const setOpened = React.useCallback((opened: boolean) => {
    Animated.timing(animHeight, { toValue: navHeight(opened), duration: 200, easing: Easing.quad }).start(() => _setOpened(v => !v));
  }, [numChildren]);
  return (
    <NavLayoutContext.Provider value={{ height, opened, setOpened, setNumChildren, animHeight, navHeight }}>
      <View style={{ minWidth: 300, marginHorizontal: 32, flex: 1, minHeight: '100vh' }}>
        <Breakable
          renderSmall={children => <>
            {props.renderNavBar && props.renderNavBar()}
            <View key={1} style={{ flex: 1 }}>{children}</View>
          </>}
          renderMedium={children => <>
            <View key={1} style={{ marginTop: height, marginBottom: -height }}>{children}</View>
            {props.renderNavBar && props.renderNavBar()}
          </>}
        >
          <View style={{ backgroundColor: rgba(Colors.black, .03), marginHorizontal: -32, paddingHorizontal: 32, flexGrow: 1 }}>
            {props.children}
          </View>
        </Breakable>
      </View>
    </NavLayoutContext.Provider>
  );
}