import * as React from 'react';
import * as UI from 'core-ui';
import { useForm } from '../hooks/useform';
import { useAccount } from '../hooks/useaccount';

export const ResetPassword = (props: {
  emailOrUsername: string,
  onGoToLogIn: () => void,
}) => {
  const { loading, resetPassword } = useAccount();

  const form = useForm({
    code: {
      type: 'text',
      message: 'Invalid code',
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
    await resetPassword(props.emailOrUsername, code, newPassword);
    props.onGoToLogIn();
  });

  return (
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
        onChangeText={form.changeText('code')}
        value={form.value('code')}
        message={form.message('code')}
        disabled={loading}
        placeholder='Code'
      />
      <UI.PasswordInput
        onChangeText={form.changeText('newPassword')}
        value={form.value('newPassword')}
        message={form.message('newPassword')}
        disabled={loading}
        placeholder='New Password'
      />
      <UI.Spacer size="md" />
      <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
        <UI.Link testID="LOGIN" size="md" disabled={loading} onPress={props.onGoToLogIn}>
          <UI.Icon size="sm" name="chevronLeft" color={UI.Colors.green} />Back to Login</UI.Link>
        <UI.Button disabled={loading} onPress={form.submit} loading={loading}>Reset Password</UI.Button>
      </UI.View>
      <UI.HRule />
    </UI.View>);
}

