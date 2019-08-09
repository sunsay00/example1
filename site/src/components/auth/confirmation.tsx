import * as React from 'react';
import * as UI from 'core-ui';
import { useConfirmationForm } from 'cf-cognito';

export const Confirmation = (props: {
  onGoToLogIn: () => void
}) => {
  const { loading } = UI.useLoading(Confirmation);
  const { form, resendConfirmationCode } = useConfirmationForm({ onGoToLogIn: props.onGoToLogIn });

  return (
    <>
      <UI.NumericInput {...form.fields.code} />
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