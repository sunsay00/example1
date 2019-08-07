import * as React from 'react';
import { useForm } from '../hooks/useform';
import * as UI from 'gatsby-theme-core-ui';
import { useAccount, SignUpResult } from '../hooks/useaccount';

export const SignUp = (props: {
  onGoToLogIn: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
  onVersion: () => void;
  role: string,
  locale: string,
  version: string;
  renderLogo?: () => JSX.Element,
}) => {
  const { loading, signUp } = useAccount();
  const toast = UI.useToast();

  const form = useForm({
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
    const result = await signUp(username, email, password, props.locale, props.role);
    if (result == SignUpResult.Success) {
      props.onGoToConfirmation(username);
    } else if (result == SignUpResult.UsernameExists) {
      toast.warn('User already exists');
    } else if (result == SignUpResult.Unknown) {
      throw new Error('unknown signup error');
    }
  });

  return (
    <UI.View
      style={{
        width: '100%',
        height: '100%',
        flexDirection: 'column',
        alignItems: 'stretch',
        justifyContent: 'center',
        flex: 1,
      }}
    >
      <UI.View style={{ justifyContent: 'center', width: '100%', alignItems: 'center' }}>
        {props.renderLogo && props.renderLogo()}
      </UI.View>
      <UI.UserNameInput
        testID="USERNAME"
        placeholder="Username"
        onChangeText={form.changeText('username')}
        value={form.value('username')}
        message={form.message('username')}
      />
      <UI.Spacer size="md" />
      <UI.EmailInput
        testID="EMAIL"
        placeholder="Email"
        onChangeText={form.changeText('email')}
        value={form.value('email')}
        message={form.message('email')}
      />
      <UI.Spacer size="md" />
      <UI.PasswordInput
        testID="PASSWORD"
        placeholder="Password"
        onChangeText={form.changeText('password')}
        value={form.value('password')}
        message={form.message('password')}
      />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button
          testID="SIGNUP"
          disabled={loading}
          loading={loading}
          onPress={form.submit}
        >Sign up</UI.Button>
      </UI.View>
      {props.onGoToLogIn && <>
        <UI.HRule />
        <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
          <UI.Text>Already have an account?</UI.Text>
          <UI.Spacer />
          <UI.Link
            testID="LOGIN"
            size="md"
            onPress={props.onGoToLogIn}
            disabled={loading}
          >Log in</UI.Link>
        </UI.View>
      </>}
    </UI.View>
  );
}