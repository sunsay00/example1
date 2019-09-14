import { createModule } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';

export const useCert = (inputs: {
  Domain: string,
}) => createModule('cf-cert', async () => {
  return await useCloudFormation({
    id: 'cf-cert',
    cfyamlpath: `${__dirname}/cf.yaml`,
    inputs: {
      Domain: inputs.Domain
    },
    defaultOutputs: {
      CertificateArn: ''
    }
  });
});
