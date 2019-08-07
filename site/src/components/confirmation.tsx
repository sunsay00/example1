import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';

export const Confirmation = (props: {
  onGoToLogIn: () => void
}) => {
  const { loading, form, resendConfirmationCode } = UI.useConfirmationForm({ onGoToLogIn: props.onGoToLogIn });

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
