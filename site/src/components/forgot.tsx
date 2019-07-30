import * as React from 'react';
import * as UI from 'core-ui';

export const Forgot = (props: {
  onSend: () => void,
  loading: boolean,
  onEmailOrUsernameChangeText: (text: string) => void,
  emailOrUsernameValue: string,
  emailOrUsernameMessage?: string,
}) =>
  <UI.View style={{
    width: '100%',
    height: '100%',
    flexDirection: 'column',
    alignItems: 'stretch',
    justifyContent: 'center',
    flex: 1,
  }}>
    <UI.View>
      <UI.Text size="lg">Forgot your password?</UI.Text>
    </UI.View>
    <UI.Spacer size="lg" />
    <UI.Text size="sm">Enter the email address or username associated with your account, and we’ll email you a code to reset your password.</UI.Text>
    <UI.Spacer size="xl" />
    <UI.UserNameInput
      placeholder='Username or Email'
      onChangeText={props.onEmailOrUsernameChangeText}
      value={props.emailOrUsernameValue}
      message={props.emailOrUsernameMessage}
    />
    <UI.Spacer size="md" />
    <UI.View style={{ width: '100%' }}>
      <UI.Button disabled={props.loading} loading={props.loading} onPress={props.onSend}>Send Recovery Email</UI.Button>
    </UI.View>
  </UI.View>