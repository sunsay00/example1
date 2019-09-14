import { createModule, useGlobals } from '@inf/hookops';
import { useDocker } from '@inf/cf-docker/config';

export const useRedis = () => createModule('cf-redis', async () => {
  const { stage } = useGlobals();
  if (stage == 'local') {
    const redis = await useDocker({
      id: 'redis',
      service: {
        ports: ['6379:6379'],
        image: 'redis:3.2.10-alpine'
      },
      outputs: { host: 'localhost:6370' }
    });

    return {
      host: redis.host,
      port: 6379
    };
  } else {
    return {
      host: '',
      port: 6379
    };
  }
});