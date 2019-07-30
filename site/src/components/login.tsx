import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export const LogIn = (props: {
  onForgot: () => void,
  onSignUp?: () => void,
  onLogIn: () => void,
  onEmailOrUsernameChangeText: (text: string) => void,
  emailOrUsernameValue: string,
  emailOrUsernameMessage?: string,
  onPasswordChangeText: (text: string) => void,
  passwordValue: string,
  passwordMessage?: string,
  loading: boolean,
  renderLogo?: () => JSX.Element,
}) =>
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
      onChangeText={props.onEmailOrUsernameChangeText}
      value={props.emailOrUsernameValue}
      message={props.emailOrUsernameMessage}
    />
    <UI.Spacer size="sm" />
    <UI.PasswordInput
      testID="PASSWORD"
      placeholder='Password'
      onChangeText={props.onPasswordChangeText}
      value={props.passwordValue}
      message={props.passwordMessage}
    />
    <UI.Spacer size="md" />
    <UI.View style={{ width: '100%' }}>
      <UI.Button testID="LOGIN" disabled={props.loading} onPress={props.onLogIn} loading={props.loading}>Log in</UI.Button>
    </UI.View>
    {props.onForgot && <>
      <UI.Spacer size="sm" />
      <UI.View style={{ alignItems: 'center' }}>
        <UI.Link testID="FORGOT" size="sm" onPress={props.onForgot} disabled={props.loading}>Forgot your password?</UI.Link>
      </UI.View>
    </>}
    {props.onSignUp && <>
      <UI.HRule />
      <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
        <UI.Text>Don't have an account?</UI.Text>
        <UI.Spacer />
        <UI.Link testID="SIGNUP" size="md" onPress={props.onSignUp} disabled={props.loading}>Sign up</UI.Link>
      </UI.View>
    </>}
  </UI.View>
