import * as React from 'react';
import * as UI from '@infng/core-ui';
import { useResetPasswordForm } from '@infng/cf-cognito';

export const ResetPassword = (props: {
  emailOrUsername: string,
  onGoToLogIn: () => void,
}) => {
  const { loading } = UI.useLoading(ResetPassword);
  const { form } = useResetPasswordForm({
    emailOrUsername: props.emailOrUsername,
    onGoToLogIn: props.onGoToLogIn
  });

  return (
    <UI.View style={{
      flex: 1,
      justifyContent: 'center',
    }} >
      <UI.Text size="sm">Please enter the code you received in your email below.</UI.Text>
      <UI.Spacer size="xl" />
      <UI.NumericInput {...form.fields.code} />
      <UI.PasswordInput {...form.fields.newPassword} />
      <UI.Spacer size="md" />
      <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
        <UI.Link testID="LOGIN" size="md" disabled={loading} onPress={props.onGoToLogIn}>
          <UI.Icon size="sm" name="arrow-back" color={UI.Colors.green} />Back to Login</UI.Link>
        <UI.Button disabled={loading} onPress={form.submit} loading={loading}>Reset Password</UI.Button>
      </UI.View>
      <UI.HRule />
    </UI.View>);
}

