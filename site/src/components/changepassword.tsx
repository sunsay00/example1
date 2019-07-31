import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { useForm } from '../hooks/useform';
import { useAccount } from '../hooks/useaccount';

export const ChangePassword = (props: {
  locale: string,
  onToGoLogIn: () => void,
}) => {
  const { loading, changePassword } = useAccount();
  const form = useForm({
    newPassword: {
      type: 'password',
      message: 'Password too short',
      pattern: /^.{8,}$/,
      default: '',
    },
  }, async ({ newPassword }) => {
    await changePassword(newPassword, props.locale);
  });

  return (
    <UI.View style={{
      width: '100%',
      height: '100%',
      flexDirection: 'column',
      alignItems: 'center',
      justifyContent: 'center',
      paddingHorizontal: 40,
      backgroundColor: UI.Colors.splash,
    }}>
      <UI.PasswordInput
        placeholder='New Password'
        onChangeText={form.changeText('newPassword')}
        value={form.value('newPassword')}
        message={form.message('newPassword')}
      />
      <UI.Spacer size="md" />
      <UI.Spacer size="md" />
      <UI.View style={{ width: '100%' }}>
        <UI.Button disabled={loading} onPress={props.onToGoLogIn} loading={loading}>Change Password</UI.Button>
      </UI.View>
    </UI.View>
  );
}