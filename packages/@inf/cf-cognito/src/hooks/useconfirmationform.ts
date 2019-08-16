import * as UI from '@inf/core-ui';
import { useAccount, ConfirmResult } from './useaccount';

export const useConfirmationForm = (props: { onGoToLogIn: () => void }) => {
  const { loading } = UI.useLoading(useConfirmationForm);
  const { resendConfirmationCode, confirmCode } = useAccount();
  const toast = UI.useToast();

  const form = UI.useForm({
    code: {
      type: 'text',
      placeholder: 'Confirmation Code',
      message: 'Invalid code',
      pattern: /.+/,
      default: '',
    }
  }, async ({ code }) => {
    const result = await confirmCode(code);
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

  return { loading, form, resendConfirmationCode };
}
