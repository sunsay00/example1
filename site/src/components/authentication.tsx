import * as React from 'react';
import { useState } from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { LogIn } from '../components/login';
import { SignUp } from '../components/signup';
import { ResetPassword } from '../components/resetpassword';
import { Forgot } from '../components/forgot';
import { Confirmation } from '../components/confirmation';
import { useLocale } from '../hooks/uselocale';
import { ChangePassword } from '../components/changepassword';

// [ STATE FLOWS ]
// signUp -> confirmation -> logIn
// logIn -> forgot -> reset -> logIn
// logIn -> changePassword -> logIn
// logIn -> [DONE]

enum AuthenticationMode { LogIn, SignUp, Confirmation, Forgot, ResetPassword, ChangePassword };

export const Authentication = (props: {
  onLogInComplete: () => void
}) => {
  const locale = useLocale();
  const [mode, setMode] = useState<AuthenticationMode>(AuthenticationMode.Confirmation);//.LogIn);
  const [emailOrUsername, setEmailOrUsername] = useState<string | undefined>();
  const [verifiedUsername, setVerifiedUsername] = useState<string | undefined>('wakka');

  if (!locale) return null;

  if (mode == AuthenticationMode.LogIn) {
    return (
      <LogIn
        onLogInComplete={() => props.onLogInComplete()}
        onGoToForgot={() => setMode(AuthenticationMode.Forgot)}
        onGoToSignUp={() => setMode(AuthenticationMode.SignUp)}
        onGoToChangePassword={() => setMode(AuthenticationMode.ChangePassword)}
        onGoToConfirmation={verifiedUsername => {
          setVerifiedUsername(verifiedUsername);
          setMode(AuthenticationMode.Confirmation);
        }}
        renderLogo={() => <UI.View />/*<UI.Image source={require('../../img/logo_sm.png')} style={{ width: 80, height: 80 }} />*/}
      />
    );
  } else if (mode == AuthenticationMode.SignUp) {
    return (
      <SignUp
        onGoToLogIn={() => setMode(AuthenticationMode.LogIn)}
        onGoToConfirmation={verifiedUsername => {
          setVerifiedUsername(verifiedUsername);
          setMode(AuthenticationMode.Confirmation);
        }}
        onVersion={() => { }}
        role="default"
        locale={locale}
        version={'0.0.1'}
      />
    );
  } else if (mode == AuthenticationMode.Confirmation && verifiedUsername) {
    return (
      <Confirmation
        onGoToLogIn={() => setMode(AuthenticationMode.LogIn)}
      />
    );
  } else if (mode == AuthenticationMode.Forgot) {
    return (
      <Forgot
        onGoToResetPassword={emailOrUsername => {
          setEmailOrUsername(emailOrUsername);
          setMode(AuthenticationMode.ResetPassword);
        }}
        onGoToLogIn={() => setMode(AuthenticationMode.LogIn)}
      />
    );
  } else if (mode == AuthenticationMode.ResetPassword && emailOrUsername) {
    return (
      <ResetPassword
        emailOrUsername={emailOrUsername}
        onGoToLogIn={() => setMode(AuthenticationMode.LogIn)}
      />
    );
  } else if (mode == AuthenticationMode.ChangePassword) {
    return (
      <ChangePassword
        locale={locale}
        onToGoLogIn={() => setMode(AuthenticationMode.LogIn)}
      />
    )
  } else {
    return null;
  }
}