import * as React from 'react';
import { useState, useEffect, useContext } from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { LogIn } from '../components/login';
import { SignUp } from '../components/signup';
import { ResetPassword } from '../components/resetpassword';
import { Forgot } from '../components/forgot';
import { useAccount } from '../hooks/useaccount';
import { useForm } from '../hooks/useform';
import { FullModalContext } from '../components/fullmodal';

export const Authentication = (props: { onLoggedIn: () => void }) => {
  const [emailOrUsername, setEmailOrUsername] = useState<string | undefined>(undefined);
  const { loading, resendConfirmation, logIn, signUp, sendRecoveryEmail, resetPassword } = useAccount();
  const [mode, setMode] = useState<'login' | 'signup' | 'forgot' | 'resetpassword'>('login');
  const { modalVisible } = useContext(FullModalContext);

  useEffect(() => {
    if (modalVisible) {
      setEmailOrUsername(undefined);
      setMode('login');
    }
  }, [modalVisible]);

  const logInForm = useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      message: 'Invalid Email or Username',
      default: '',
    },
    password: {
      type: 'password',
      pattern: /^.{8,}$/,
      message: 'Password too short',
      default: '',
    },
  }, async ({ emailOrUsername, password }) => {
    await logIn(emailOrUsername, password);
    props.onLoggedIn();
  });

  const signUpForm = useForm({
    username: {
      type: 'username',
      message: 'Invalid Username',
      default: '',
    },
    email: {
      type: 'email',
      message: 'Invalid Email address',
      default: '',
    },
    password: {
      type: 'password',
      pattern: /^.{8,}$/,
      message: 'Password too short',
      default: '',
    },
  }, async ({ username, email, password }) => {
    await signUp(username, email, password);
    setMode('login');
  });

  const forgotForm = useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      message: 'Invalid Email or Username',
      default: '',
    },
  }, async ({ emailOrUsername }) => {
    await sendRecoveryEmail(emailOrUsername);
    setEmailOrUsername(emailOrUsername);
    setMode('resetpassword');
  });

  const resetPasswordForm = useForm({
    code: {
      type: 'text',
      message: 'Invalid Username',
      pattern: /.+/,
      default: '',
    },
    newPassword: {
      type: 'password',
      message: 'Password too short',
      pattern: /^.{8,}$/,
      default: '',
    },
  }, async ({ code, newPassword }) => {
    if (!emailOrUsername) throw new Error('invalid emailOrUsername');
    await resetPassword(emailOrUsername, code, newPassword);
    setMode('login');
  });

  if (mode == 'login') {
    return (
      <LogIn
        loading={loading}
        onLogIn={() => logInForm.submit()}
        onForgot={() => setMode('forgot')}
        onSignUp={() => setMode('signup')}
        onEmailOrUsernameChangeText={logInForm.changeText('emailOrUsername')}
        emailOrUsernameValue={logInForm.value('emailOrUsername')}
        emailOrUsernameMessage={logInForm.message('emailOrUsername')}
        onPasswordChangeText={logInForm.changeText('password')}
        passwordValue={logInForm.value('password')}
        passwordMessage={logInForm.message('password')}
        renderLogo={() =>
          <UI.View />//<UI.Image source={require('../../img/logo_sm.png')} style={{ width: 80, height: 80 }} />
        }
      />
    );
  } else if (mode == 'signup') {
    return (
      <SignUp
        loading={loading}
        onSignUp={() => signUpForm.submit()}
        onLogIn={() => setMode('login')}
        onUsernameChangeText={signUpForm.changeText('username')}
        usernameValue={signUpForm.value('username')}
        usernameMessage={signUpForm.message('username')}
        onEmailChangeText={signUpForm.changeText('email')}
        emailValue={signUpForm.value('email')}
        emailMessage={signUpForm.message('email')}
        onPasswordChangeText={signUpForm.changeText('password')}
        passwordValue={signUpForm.value('password')}
        passwordMessage={signUpForm.message('password')}
        onVersion={() => { }}
        version={'0.0.1'}
      />
    );
  } else if (mode == 'forgot') {
    return (
      <Forgot
        loading={loading}
        onSend={() => sendRecoveryEmail}
        onLogIn={() => setMode('login')}
        onEmailOrUsernameChangeText={forgotForm.changeText('emailOrUsername')}
        emailOrUsernameValue={forgotForm.value('emailOrUsername')}
        emailOrUsernameMessage={forgotForm.message('emailOrUsername')}
      />
    );
  } else if (mode == 'resetpassword') {
    return (
      <ResetPassword
        loading={loading}
        onReset={() => resetPasswordForm.submit()}
        onLogIn={() => setMode('login')}
        onResend={() => {
          if (!emailOrUsername) throw new Error('invalid emailOrUsername');
          resendConfirmation(emailOrUsername);
        }}
        onCodeChangeText={resetPasswordForm.changeText('code')}
        codeValue={resetPasswordForm.value('code')}
        codeMessage={resetPasswordForm.message('code')}
        onNewPasswordChangeText={resetPasswordForm.changeText('newPassword')}
        newPasswordValue={resetPasswordForm.value('newPassword')}
        newPasswordMessage={resetPasswordForm.message('newPassword')}
      />
    )
  } else {
    return null;
  }
}