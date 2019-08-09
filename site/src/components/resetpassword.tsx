import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { useResetPasswordForm } from 'cf-cognito';

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
      width: '100%',
      height: '100%',
      flexDirection: 'column',
      alignItems: 'stretch',
      justifyContent: 'center',
      flex: 1,
    }} >
      <UI.Text size="sm">Please enter the code you received in your email below.</UI.Text>
      <UI.Spacer size="xl" />
      <UI.NumericInput {...form.fields.code} />
      <UI.PasswordInput {...form.fields.newPassword} />
      <UI.Spacer size="md" />
      <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
        <UI.Link testID="LOGIN" size="md" disabled={loading} onPress={props.onGoToLogIn}>
          <UI.Icon size="sm" name="chevronLeft" color={UI.Colors.green} />Back to Login</UI.Link>
        <UI.Button disabled={loading} onPress={form.submit} loading={loading}>Reset Password</UI.Button>
      </UI.View>
      <UI.HRule />
    </UI.View>);
}

