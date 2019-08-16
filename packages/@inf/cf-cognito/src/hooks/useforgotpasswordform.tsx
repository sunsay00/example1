import { useAccount } from './useaccount';
import * as UI from '@inf/core-ui';

export const useForgotPasswordForm = (props: {
  onGoToResetPassword: (emailOrUsername: string) => void,
}) => {
  const { loading } = UI.useLoading(useForgotPasswordForm);
  const { sendRecoveryEmail } = useAccount();
  const form = UI.useForm({
    emailOrUsername: {
      type: 'text',
      pattern: /^.{2,}$/,
      placeholder: 'Email or Username',
      message: 'Invalid Email or Username',
      default: '',
    },
  }, async ({ emailOrUsername }) => {
    await sendRecoveryEmail(emailOrUsername);
    props.onGoToResetPassword(emailOrUsername);
  });

  return { loading, form };
}