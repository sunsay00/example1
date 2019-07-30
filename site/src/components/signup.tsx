import * as React from 'react';
import * as UI from 'core-ui';

export const SignUp = (props: {
  onSignUp: () => void;
  loading: boolean;
  onLogIn?: () => void,
  onUsernameChangeText: (text: string) => void;
  usernameValue: string;
  usernameMessage?: string;
  onEmailChangeText: (text: string) => void;
  emailValue: string;
  emailMessage?: string;
  onPasswordChangeText: (text: string) => void;
  passwordValue: string;
  passwordMessage?: string;
  onVersion: () => void;
  version: string;
  renderLogo?: () => JSX.Element,
}) =>
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
      onChangeText={props.onUsernameChangeText}
      value={props.usernameValue}
      message={props.usernameMessage}
    />
    <UI.Spacer size="md" />
    <UI.EmailInput
      testID="EMAIL"
      placeholder="Email"
      onChangeText={props.onEmailChangeText}
      value={props.emailValue}
      message={props.emailMessage}
    />
    <UI.Spacer size="md" />
    <UI.PasswordInput
      testID="PASSWORD"
      placeholder="Password"
      onChangeText={props.onPasswordChangeText}
      value={props.passwordValue}
      message={props.passwordMessage}
    />
    <UI.Spacer size="md" />
    <UI.View style={{ width: '100%' }}>
      <UI.Button
        testID="SIGNUP"
        disabled={props.loading}
        loading={props.loading}
        onPress={props.onSignUp}
      >Sign up</UI.Button>
    </UI.View>
    {props.onLogIn && <>
      <UI.HRule />
      <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
        <UI.Text>Already have an account?</UI.Text>
        <UI.Link testID="LOGIN" size="md" onPress={props.onLogIn} disabled={props.loading}>Log in</UI.Link>
      </UI.View>
    </>}
  </UI.View>