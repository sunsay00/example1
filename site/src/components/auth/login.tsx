import * as React from 'react';
import * as UI from 'core-ui';
import { useLogInForm } from 'cf-cognito';

export const LogIn = (props: {
  onLogInComplete: () => void,
  onGoToForgot?: () => void,
  onGoToChangePassword: () => void,
  onGoToSignUp?: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
}) => {
  const { loading } = UI.useLoading(LogIn);
  const { form } = useLogInForm({
    onLogInComplete: props.onLogInComplete,
    onGoToChangePassword: props.onGoToChangePassword,
    onGoToConfirmation: props.onGoToConfirmation,
  });

  return (
    <UI.View style={{
      flex: 1,
      justifyContent: 'center',
    }}>
      <UI.UserNameInput {...form.fields.emailOrUsername} />
      <UI.Spacer size="sm" />
      <UI.PasswordInput {...form.fields.password} />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button testID="LOGIN" disabled={loading} onPress={form.submit} loading={loading}>Log in</UI.Button>
      </UI.View>
      {props.onGoToForgot && <>
        <UI.Spacer size="sm" />
        <UI.View style={{ alignItems: 'center' }}>
          <UI.Link testID="FORGOT" size="sm" onPress={props.onGoToForgot} disabled={loading}>Forgot your password?</UI.Link>
        </UI.View>
      </>}
      {props.onGoToSignUp && <>
        <UI.HRule />
        <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
          <UI.Text>Don't have an account?</UI.Text>
          <UI.Spacer />
          <UI.Link testID="SIGNUP" size="md" onPress={props.onGoToSignUp} disabled={loading}>Sign up</UI.Link>
        </UI.View>
      </>}
    </UI.View>
  );
}
