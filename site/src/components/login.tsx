import * as React from 'react';
import { useForm } from '../hooks/useform';
import * as UI from 'gatsby-theme-core-ui';
import { useAccount, LogInResult } from '../hooks/useaccount';
import { useToast } from '../hooks/usetoast';

export const LogIn = (props: {
  onLogInComplete: () => void,
  onGoToForgot?: () => void,
  onGoToChangePassword: () => void,
  onGoToSignUp?: () => void,
  renderLogo?: () => JSX.Element,
}) => {
  const { loading, logIn, resendConfirmationCode } = useAccount();
  const toast = useToast();
  const form = useForm({
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
    const result = await logIn(emailOrUsername, password);
    if (result == LogInResult.Success) {
      props.onLogInComplete();
    } else if (result == LogInResult.ChangePassword) {
      props.onGoToChangePassword();
    } else if (result == LogInResult.UserNotFound) {
      toast.warn('User not found');
    } else if (result == LogInResult.NotAuthorized) {
      toast.warn('User not authorized');
    } else if (result == LogInResult.UserNotConfirmed) {
      UI.Alert.alert('Unconfirmed user', 'This user has not been confirmed, resend confirmation code?', [
        { text: 'Cancel', onPress: () => { } },
        { text: 'Resend', onPress: () => resendConfirmationCode() },
      ])
    } else if (result == LogInResult.Unknown) {
      throw new Error('unknown signup error');
    }
  });

  return (
    <UI.View style={{
      width: '100%',
      height: '100%',
      flexDirection: 'column',
      alignItems: 'stretch',
      justifyContent: 'center',
      flex: 1,
    }}>
      <UI.View style={{ justifyContent: 'center', width: '100%', alignItems: 'center' }}>
        {props.renderLogo && props.renderLogo()}
      </UI.View>
      <UI.UserNameInput
        testID="EMAIL_OR_USERNAME"
        placeholder='Email or Username'
        onChangeText={form.changeText('emailOrUsername')}
        value={form.value('emailOrUsername')}
        message={form.message('emailOrUsername')}
      />
      <UI.Spacer size="sm" />
      <UI.PasswordInput
        testID="PASSWORD"
        placeholder='Password'
        onChangeText={form.changeText('password')}
        value={form.value('password')}
        message={form.message('password')}
      />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button testID="LOGIN" disabled={loading} onPress={form.submit} loading={loading}>Log in</UI.Button>
      </UI.View>
      {props.onGoToForgot && <>
        <UI.Spacer size="sm" />
        <UI.View style={{ alignItems: 'center' }}>
          <UI.Link testID="FORGOT" size="sm" onPress={props.onGoToForgot} disabled={loading}>Forgot your password?</UI.Link>
        </UI.View>
      </>}
      {props.onGoToSignUp && <>
        <UI.HRule />
        <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
          <UI.Text>Don't have an account?</UI.Text>
          <UI.Spacer />
          <UI.Link testID="SIGNUP" size="md" onPress={props.onGoToSignUp} disabled={loading}>Sign up</UI.Link>
        </UI.View>
      </>}
    </UI.View>
  );
}
