import * as React from 'react';
import * as UI from 'core-ui';
import { useSignUpForm } from 'cf-cognito';

export const SignUp = (props: {
  onGoToLogIn: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
  onVersion: () => void;
  role: string,
  locale: string,
  version: string;
}) => {
  const { loading } = UI.useLoading(SignUp);
  const { form } = useSignUpForm({
    onGoToConfirmation: props.onGoToConfirmation,
    role: props.role,
    locale: props.locale,
  });

  return (
    <UI.View style={{
      flex: 1,
      justifyContent: 'center',
      paddingTop: 80,
      paddingHorizontal: 40,
    }} >
      <UI.UserNameInput {...form.fields.username} />
      <UI.Spacer size="md" />
      <UI.EmailInput {...form.fields.email} />
      <UI.Spacer size="md" />
      <UI.PasswordInput {...form.fields.password} />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button testID="SIGNUP" disabled={loading} loading={loading} onPress={form.submit}>Sign up</UI.Button>
      </UI.View>
      {props.onGoToLogIn && <>
        <UI.HRule />
        <UI.View style={{ flexDirection: 'row', alignItems: 'baseline' }}>
          <UI.Text>Already have an account?</UI.Text>
          <UI.Spacer />
          <UI.Link testID="LOGIN" size="md" onPress={props.onGoToLogIn} disabled={loading}>Log in</UI.Link>
        </UI.View>
      </>}
    </UI.View>
  );
}