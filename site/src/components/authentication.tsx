import * as React from 'react';
import { useState } from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { LogIn } from '../components/login';

import { useForm } from '../hooks/useform';

const useToast = () => {
  return {
    info: (msg: string) => {
      console.log(msg);
    },
    error: (msg: string) => {
      console.error(msg);
    },
  };
}

export type SignInProps = {
  onBack: () => void,
  onRegisterDealership: () => void,
  onForgot: () => void,
  onContinue: (username: string, password: string) => Promise<void>,
  onNeedsConfirmation: (username: string) => void,
};

const useLogIn = () => {
  const toast = useToast();
  const [loading, setLoading] = useState(false);
  const resendConfirmation = async (username: string) => {
    /*
    try {
      setLoading(true);
      await Root.account().resendConfirmationCode();
      this.props.onNeedsConfirmation(username);
    } catch (err) {
      Toast.error(err);
    } finally {
      setLoading(false);
    }
    */
  }

  const logIn = async (emailOrUsername: string, password: string) => {
    /*
    try {
      setLoading(true);
      await this.props.onContinue(emailOrUsername.trim(), password);
    } catch (err) {
      if (err.code === 'UserNotFoundException' || err.code === 'NotAuthorizedException') {
        toast.info(err.message);
      } else if (err.code === 'UserNotConfirmedException') {
        UI.Alert.alert('User not confirmed', 'This user has not been confirmed, resend confirmation code?', [
          { text: 'Cancel', onPress: () => { } },
          { text: 'Resend', onPress: () => this.onResendConfirmation(values.emailOrUsername) },
        ])
      } else {
        toast.info(`${err.message} (${err.code})`);
      }
    } finally {
      setLoading(false);
    }
    */
  };

  return { loading, resendConfirmation, logIn };
}

//export const emailPattern = /^(([^<>()\[\]\.,;:\s@\"]+(\.[^<>()\[\]\.,;:\s@\"]+)*)|(\".+\"))@(([^<>()[\]\.,;:\s@\"]+\.)+[^<>()[\]\.,;:\s@\"]{2,})$/i;
//export const usernamePattern = /^[a-zA-Z0-9_-]{2,30}$/;

export const Authentication = (props: {}) => {
  const form = useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      message: 'Invalid Email or Username',
      default: '',
    },
    password: {
      type: 'password',
      pattern: /.{8,}/,
      message: 'Password too short',
      default: '',
    },
  } as const);

  const { loading, resendConfirmation, logIn } = useLogIn();
  const [mode, setMode] = useState<'login'>('login');

  if (mode == 'login') {
    return (
      <LogIn
        onForgot={() => resendConfirmation('')}
        onLogIn={() => {
          const ret = form.validate();
          ret && logIn(ret.emailOrUsername, ret.password);
        }}
        onSignUp={() => { }}
        onEmailOrUsernameChangeText={form.changeText('emailOrUsername')}
        emailOrUsernameValue={form.value('emailOrUsername')}
        emailOrUsernameMessage={form.message('emailOrUsername')}
        onPasswordChangeText={form.changeText('password')}
        passwordValue={form.value('password')}
        passwordMessage={form.message('password')}
        loading={loading}
        renderLogo={() =>
          <UI.View />//<UI.Image source={require('../../img/logo_sm.png')} style={{ width: 80, height: 80 }} />
        }
      />
    );
  } else {
    return null;
  }
}