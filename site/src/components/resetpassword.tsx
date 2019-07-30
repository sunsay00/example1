import * as React from 'react';
import * as UI from 'core-ui';

// register -> confirm -> in
// signin -> in
// signin -> forgot -> reset -> 
// signin -> change password -> in

export const ResetPassword = (props: {
  loading: boolean,
  onReset: () => void,
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
    <UI.View style={{ width: '100%' }}>
      <UI.Button disabled={props.loading} onPress={props.onReset} loading={props.loading}>Reset Password</UI.Button>
    </UI.View>
  </UI.View>

