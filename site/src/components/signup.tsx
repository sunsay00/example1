import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export const SignUp = (props: {
  onGoToLogIn: () => void,
  onGoToConfirmation: (verifiedUsername: string) => void,
  onVersion: () => void;
  role: string,
  locale: string,
  version: string;
  renderLogo?: () => JSX.Element,
}) => {
  const { loading } = UI.useLoading(SignUp);
  const { form } = UI.useSignUpForm({
    onGoToConfirmation: props.onGoToConfirmation,
    role: props.role,
    locale: props.locale,
  });

  return (
    <UI.View style={{ width: '100%', height: '100%', flexDirection: 'column', alignItems: 'stretch', justifyContent: 'center', flex: 1 }} >
      <UI.View style={{ justifyContent: 'center', width: '100%', alignItems: 'center' }}>
        {props.renderLogo && props.renderLogo()}
      </UI.View>
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