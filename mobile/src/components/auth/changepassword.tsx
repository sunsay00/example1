import * as React from 'react';
import * as UI from 'core-ui';
import { useChangePasswordForm } from 'cf-cognito';

export const ChangePassword = (props: {
  locale: string,
  onToGoLogIn: () => void,
}) => {
  const { loading } = UI.useLoading(ChangePassword);
  const { form } = useChangePasswordForm({ locale: props.locale });

  return (
    <UI.View style={{
      flex: 1,
      justifyContent: 'center',
      paddingTop: 80,
      paddingHorizontal: 40,
    }}>
      <UI.PasswordInput {...form.fields.newPassword} />
      <UI.Spacer size="md" />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button disabled={loading} onPress={props.onToGoLogIn} loading={loading}>Change Password</UI.Button>
      </UI.View>
    </UI.View>
  );
}