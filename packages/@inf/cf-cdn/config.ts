import { createModule, useGlobals } from '@inf/hookops';
import { useCloudFormation } from '@inf/hooks';

export const useCDN = (props: {
  SiteCertificateArn: string,
  Domain: string,
  HostedZoneId: string
}) => createModule(async () => {
  const { stage } = useGlobals();

  await useCloudFormation({
    id: 'cf-cdn',
    cfyamlpath: `${__dirname}/cf.yaml`,
    inputs: {
      SiteCertificateArn: props.SiteCertificateArn,
      Domain: props.Domain,
      HostedZoneId: props.HostedZoneId,
      Stage: stage
    }
  });

  return {};
})
