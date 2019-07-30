import * as React from 'react';
import * as UI from 'core-ui';

// register -> confirm -> in
// signin -> in
// signin -> forgot -> reset -> 
// signin -> change password -> in

export const ResetPassword = (props: {
  loading: boolean,
  onReset: () => void,
  onLogIn: () => void,
  onResend: () => void,
  onCodeChangeText: (text: string) => void,
  codeValue: string,
  codeMessage?: string,
  onNewPasswordChangeText: (text: string) => void,
  newPasswordValue: string,
  newPasswordMessage?: string,
}) =>
  <UI.View style={{
    width: '100%',
    height: '100%',
    flexDirection: 'column',
    alignItems: 'stretch',
    justifyContent: 'center',
    flex: 1,
  }} >
    <UI.Text size="sm">Please enter the code you received in your email below.</UI.Text>
    <UI.Spacer size="xl" />
    <UI.NumericInput
      onChangeText={props.onCodeChangeText}
      value={props.codeValue}
      message={props.codeMessage}
      disabled={props.loading}
      placeholder='Code'
    />
    <UI.PasswordInput
      onChangeText={props.onNewPasswordChangeText}
      value={props.newPasswordValue}
      message={props.newPasswordMessage}
      disabled={props.loading}
      placeholder='New Password'
    />
    <UI.Spacer size="md" />
    <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
      <UI.Link testID="LOGIN" size="md" disabled={props.loading} onPress={props.onLogIn}>
        <UI.Icon size="sm" name="chevronLeft" color={UI.Colors.green} />Back to Login</UI.Link>
      <UI.Button disabled={props.loading} onPress={props.onReset} loading={props.loading}>Reset Password</UI.Button>
    </UI.View>
    <UI.HRule />
    <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
      <UI.Text>Didn't receive an email?</UI.Text>
      <UI.Spacer />
      <UI.Link testID="RESEND" size="md" onPress={props.onResend} disabled={props.loading}>Resend Email</UI.Link>
    </UI.View>
  </UI.View>

