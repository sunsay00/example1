import * as UI from 'core-ui';
import { useAccount } from './useaccount';

export const useResetPasswordForm = (props: { emailOrUsername: string, onGoToLogIn: () => void }) => {
  const { loading } = UI.useLoading(useResetPasswordForm);
  const { resetPassword } = useAccount();

  const form = UI.useForm({
    code: {
      type: 'text',
      placeholder: 'Code',
      message: 'Invalid code',
      pattern: /.+/,
      default: '',
    },
    newPassword: {
      type: 'password',
      placeholder: 'New Password',
      message: 'Password too short',
      pattern: /^.{8,}$/,
      default: '',
    },
  }, async ({ code, newPassword }) => {
    await resetPassword(props.emailOrUsername, code, newPassword);
    props.onGoToLogIn();
  });

  return { loading, form };
}
