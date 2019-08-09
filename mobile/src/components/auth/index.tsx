import * as React from 'react';
import { useState } from 'react';
import * as UI from 'core-ui';
import { LogIn } from './login';
import { SignUp } from './signup';
import { ResetPassword } from './resetpassword';
import { ForgotPassword } from './forgotpassword';
import { Confirmation } from './confirmation';
import { ChangePassword } from './changepassword';

const useLocale = () => 'EN';

// [ STATE FLOWS ]
// signUp -> confirmation -> logIn
// logIn -> forgot -> reset -> logIn
// logIn -> changePassword -> logIn
// logIn -> [DONE]

enum AuthMode { LogIn, SignUp, Confirmation, Forgot, ResetPassword, ChangePassword };

export const Auth = (props: {
  onLogInComplete: () => void
}) => {
  const locale = useLocale();
  const [mode, setMode] = useState<AuthMode>(AuthMode.LogIn);
  const [name, setName] = useState<string>();

  if (!locale) return <UI.Loading />;

  if (mode == AuthMode.LogIn) {
    return (
      <LogIn
        onLogInComplete={() => props.onLogInComplete()}
        onGoToForgot={() => setMode(AuthMode.Forgot)}
        onGoToSignUp={() => setMode(AuthMode.SignUp)}
        onGoToChangePassword={() => setMode(AuthMode.ChangePassword)}
        onGoToConfirmation={verifiedUsername => {
          setName(verifiedUsername);
          setMode(AuthMode.Confirmation);
        }}
        renderLogo={() => <UI.View />/*<UI.Image source={require('../../img/logo_sm.png')} style={{ width: 80, height: 80 }} />*/}
      />
    );
  } else if (mode == AuthMode.SignUp) {
    return (
      <SignUp
        onGoToLogIn={() => setMode(AuthMode.LogIn)}
        onGoToConfirmation={verifiedUsername => {
          setName(verifiedUsername);
          setMode(AuthMode.Confirmation);
        }}
        onVersion={() => { }}
        role="default"
        locale={locale}
        version={'0.0.1'}
      />
    );
  } else if (mode == AuthMode.Confirmation && name) {
    return (
      <Confirmation
        onGoToLogIn={() => setMode(AuthMode.LogIn)}
      />
    );
  } else if (mode == AuthMode.Forgot) {
    return (
      <ForgotPassword
        onGoToResetPassword={emailOrUsername => {
          setName(emailOrUsername);
          setMode(AuthMode.ResetPassword);
        }}
        onGoToLogIn={() => setMode(AuthMode.LogIn)}
      />
    );
  } else if (mode == AuthMode.ResetPassword && name) {
    return (
      <ResetPassword
        emailOrUsername={name}
        onGoToLogIn={() => setMode(AuthMode.LogIn)}
      />
    );
  } else if (mode == AuthMode.ChangePassword) {
    return (
      <ChangePassword
        locale={locale}
        onToGoLogIn={() => setMode(AuthMode.LogIn)}
      />
    )
  } else {
    return null;
  }
}
/*
  Auth: createStackNavigator({
    LogIn: () => {
      const nav = useNavigation();
      return (
        <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
          <UI.Header1>Log in</UI.Header1>
          <UI.Button onPress={() => nav.navigate('Main')}>Sign in</UI.Button>
          <UI.Spacer />
          <UI.Button onPress={() => nav.push('SignUp')}>Sign up</UI.Button>
          <UI.Spacer />
          <UI.Button size="xs" secondary onPress={() => nav.navigate('Landing')}>Landing</UI.Button>
        </UI.View>
      );
    },
    SignUp: () => {
      const nav = useNavigation();
      return (
        <UI.View style={{ flex: 1, justifyContent: 'center', alignItems: 'center' }}>
          <UI.Header1>Sign up</UI.Header1>
          <UI.Button onPress={() => nav.navigate('LogIn')}>Go To Log in</UI.Button>
        </UI.View>
      );
    }
*/