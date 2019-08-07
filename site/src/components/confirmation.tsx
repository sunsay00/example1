import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { useForm } from '../hooks/useform';
import { useAccount, ConfirmResult } from '../hooks/useaccount';

export const Confirmation = (props: {
  onGoToLogIn: () => void
}) => {
  const { loading, resendConfirmationCode, confirm } = useAccount();
  const toast = UI.useToast();

  const form = useForm({
    code: {
      type: 'text',
      message: 'Invalid code',
      pattern: /.+/,
      default: '',
    }
  }, async ({ code }) => {
    const result = await confirm(code);
    if (result == ConfirmResult.Success) {
      toast.info('Your account has been successfully confirmed', () =>
        props.onGoToLogIn());
    } else if (result == ConfirmResult.CodeMismatch) {
      toast.warn('Your confirmation code did not match our records');
    } else if (result == ConfirmResult.Unknown) {
      throw new Error('unknown signup error');
    } else {
      throw new Error(`unhandled signup error (${result})`);
    }
  });

  return (
    <>
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
    </>
  );
}
