import { createModule } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';

export const useCert = (props: {
  Domain: string,
}) => createModule(async () => {
  return await useCloudFormation({
    id: 'cf-cert',
    cfyamlpath: `${__dirname}/cf.yaml`,
    inputs: {
      Domain: props.Domain
    },
    defaultOutputs: {
      CertificateArn: ''
    }
  });
});
