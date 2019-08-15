import * as React from 'react';
import { LogIn } from './login';
import { SignUp } from './signup';
import { ResetPassword } from './resetpassword';
import { ForgotPassword } from './forgotpassword';
import { Confirmation } from './confirmation';
import { ChangePassword } from './changepassword';
import { createStackNavigator, useNav } from '../../hooks/usenav';

const useLocale = () => 'EN';

// [ STATE FLOWS ]
// signUp -> confirmation -> logIn
// logIn -> forgot -> reset -> logIn
// logIn -> changePassword -> logIn
// logIn -> [DONE]

export const Screens = {
  LogInScreen: (props: {}) => {
    const nav = useNav();
    return (
      <LogIn
        onLogInComplete={() => nav.navigate('Main')}
        onGoToForgot={() => nav.push('ForgotPassword')}
        onGoToSignUp={() => nav.push('SignUp')}
        onGoToChangePassword={() => nav.push('ChangePassword')}
        onGoToConfirmation={verifiedUsername => {
          nav.push('Confirmation', { name: verifiedUsername });
        }}
      />
    );
  },

  SignUpScreen: (props: {}) => {
    const nav = useNav();
    const locale = useLocale();
    return (
      <SignUp
        onGoToLogIn={() => nav.popToTop()}
        onGoToConfirmation={verifiedUsername => {
          nav.push('Confirmation', { name: verifiedUsername });
        }}
        onVersion={() => { }}
        role="default"
        locale={locale}
        version={'0.0.1'}
      />
    );
  },
  ConfirmationScreen: (props: {}) => {
    const nav = useNav();
    return (
      <Confirmation
        onGoToLogIn={() => nav.push('LogIn')}
      />
    );
  },
  ForgotPasswordScreen: (props: {}) => {
    const nav = useNav();
    return (
      <ForgotPassword
        onGoToResetPassword={emailOrUsername => {
          nav.push('ResetPassword', { name: emailOrUsername });
        }}
      />
    );
  },
  ResetPasswordScreen: (props: {}) => {
    const nav = useNav();
    return (
      <ResetPassword
        emailOrUsername={nav.getParam('name')}
        onGoToLogIn={() => nav.popToTop()}
      />
    );
  },
};

export const ChangePasswordScreen = (props: {}) => {
  const nav = useNav();
  const locale = useLocale();
  return (
    <ChangePassword
      locale={locale}
      onToGoLogIn={() => nav.popToTop()}
    />
  );
};