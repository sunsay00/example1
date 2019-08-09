import * as React from 'react';
import * as UI from 'core-ui';
import { useForgotPasswordForm } from 'cf-cognito';

export const ForgotPassword = (props: {
  onGoToResetPassword: (emailOrUsername: string) => void,
  onGoToLogIn?: () => void,
}) => {
  const { loading } = UI.useLoading(ForgotPassword);
  const { form } = useForgotPasswordForm({ onGoToResetPassword: props.onGoToResetPassword });

  return (
    <UI.View style={{
      flex: 1,
      justifyContent: 'center',
      paddingHorizontal: 40,
    }}>
      <UI.View>
        <UI.Text size="lg">Forgot your password?</UI.Text>
      </UI.View>
      <UI.Spacer size="lg" />
      <UI.Text size="sm">Enter the email address or username associated with your account, and we’ll email you a code to reset your password.</UI.Text>
      <UI.Spacer size="xl" />
      <UI.UserNameInput {...form.fields.emailOrUsername} />
      <UI.Spacer size="md" />
      <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between', alignItems: 'center' }}>
        {props.onGoToLogIn && <>
          <UI.Link testID="LOGIN" size="md" disabled={loading} onPress={props.onGoToLogIn}>
            <UI.Icon size="sm" name="chevronLeft" color={UI.Colors.green} />Back to Login</UI.Link>
        </>}
        <UI.Button disabled={loading} loading={loading} onPress={form.submit}>Send Recovery Email</UI.Button>
      </UI.View>
    </UI.View>
  );
}