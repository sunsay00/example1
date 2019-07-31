import * as React from 'react';
import * as UI from 'core-ui';
import { useForm } from '../hooks/useform';
import { useAccount } from '../hooks/useaccount';

export const Forgot = (props: {
  onGoToResetPassword: (emailOrUsername: string) => void,
  onGoToLogIn?: () => void,
}) => {
  const { loading, sendRecoveryEmail } = useAccount();
  const form = useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      message: 'Invalid Email or Username',
      default: '',
    },
  }, async ({ emailOrUsername }) => {
    await sendRecoveryEmail(emailOrUsername);
    props.onGoToResetPassword(emailOrUsername);
  });

  return (
    <UI.View style={{
      width: '100%',
      height: '100%',
      flexDirection: 'column',
      alignItems: 'stretch',
      justifyContent: 'center',
      flex: 1,
    }}>
      <UI.View>
        <UI.Text size="lg">Forgot your password?</UI.Text>
      </UI.View>
      <UI.Spacer size="lg" />
      <UI.Text size="sm">Enter the email address or username associated with your account, and we’ll email you a code to reset your password.</UI.Text>
      <UI.Spacer size="xl" />
      <UI.UserNameInput
        placeholder='Username or Email'
        onChangeText={form.changeText('emailOrUsername')}
        value={form.value('emailOrUsername')}
        message={form.message('emailOrUsername')}
      />
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