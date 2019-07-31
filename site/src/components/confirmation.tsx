import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { useForm } from '../hooks/useform';
import { useAccount } from '../hooks/useaccount';

export const Confirmation = (props: {
  onGoToLogIn: () => void
}) => {
  const { loading, resendConfirmationCode, confirmSignUp } = useAccount();

  const form = useForm({
    code: {
      type: 'text',
      message: 'Invalid Username',
      pattern: /.+/,
      default: '',
    }
  }, async ({ code }) => {
    await confirmSignUp(code);
    props.onGoToLogIn();
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
      <UI.NumericInput
        testID="CODE"
        onChangeText={form.changeText('code')}
        value={form.value('code')}
        message={form.message('code')}
        disabled={loading}
        placeholder='Confirmation Code'
      />
      <UI.Spacer size='md' />
      <UI.View style={{ width: '100%' }}>
        <UI.Button testID="CONFIRM" loading={loading} disabled={loading} onPress={form.submit}>
          Confirm
        </UI.Button>
      </UI.View>
      <UI.Spacer size='md' />
      <UI.View style={{ width: '100%' }}>
        <UI.Link
          testID='RESEND'
          disabled={loading}
          onPress={() => resendConfirmationCode()}
        >Resend Confirmation Code</UI.Link>
      </UI.View>
    </UI.View>
  );
}
