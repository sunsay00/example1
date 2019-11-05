import * as React from 'react';
import * as UI from '@infng/core-ui';
import { useChangePasswordForm } from '@infng/cf-cognito';

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
