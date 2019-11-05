import * as React from 'react';
import { useState } from 'react';
import * as UI from '@infng/core-ui';
import { LogIn } from './login';
import { SignUp } from './signup';
import { ResetPassword } from './resetpassword';
import { ForgotPassword } from './forgotpassword';
import { Confirmation } from './confirmation';
import { useLocale } from '../../hooks/uselocale';
import { ChangePassword } from './changepassword';

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
