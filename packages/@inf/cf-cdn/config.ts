import { createModule, useGlobals } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';

export const useCDN = (inputs: {
  SiteCertificateArn: string,
  Domain: string,
  HostedZoneId: string
}) => createModule(__dirname, async () => {
  const { stage } = useGlobals();

  await useCloudFormation({
    id: 'cf-cdn',
    cfyamlpath: `${__dirname}/cf.yaml`,
    inputs: {
      SiteCertificateArn: inputs.SiteCertificateArn,
      Domain: inputs.Domain,
      HostedZoneId: inputs.HostedZoneId,
      Stage: stage
    }
  });

  return {};
})
