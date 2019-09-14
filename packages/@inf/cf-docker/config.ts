import * as fs from 'fs';
import * as path from 'path';
import * as yamljs from 'yamljs';
import * as RT from 'runtypes';
import {
  Outputs, createModule, vartools,
  useShell, useEffect, useTempDir, useScriptRegistry
} from '@inf/hookops';

const DockerRecord = RT.Record({
  version: RT.String,
  services: RT.Dictionary(RT.Record({
    container_name: RT.String,
    ports: RT.Array(RT.String),
    image: RT.String,
  }).And(RT.Partial({
    environment: RT.Array(RT.String)
  }))),
});

export const useDocker = <R extends Outputs>(inputs: {
  id: string,
  outputs: R
  service: {
    ports: string[],
    image: string,
    environment?: string[]
  },
}) => createModule('cf-docker', async () => {
  const localname = `${path.basename(__dirname)}${inputs.id ? `--${inputs.id}` : ''}`;

  const tmpdir = useTempDir(inputs.id);

  useScriptRegistry(`docker-${inputs.id}`, {
    cwd: tmpdir,
    rules: {
      up: {
        desc: 'start docker',
        commands: [{ command: 'docker-compose', args: ['up', '-d', '2>&1'] }]
      },
      down: {
        desc: 'stop docker',
        commands: [{ command: 'docker-compose', args: ['down', '2>&1'] }]
      }
    }
  });

  await useEffect(async () => {
    const dockerServices = {
      version: '2',
      services: {
        [localname]: {
          container_name: localname,
          ports: inputs.service.ports,
          image: inputs.service.image,
          ...(inputs.service.environment && { environment: inputs.service.environment.map(vartools.convertToShell) } || {})
        }
      }
    };

    if (dockerServices && !DockerRecord.guard(dockerServices))
      throw new Error('invalid docker config');

    if (!dockerServices) {
      fs.existsSync(`${tmpdir}/docker-compose.yml`) && fs.unlinkSync(`${tmpdir}/docker-compose.yml`);
    } else {
      fs.writeFileSync(`${tmpdir}/docker-compose.yml`, yamljs.stringify(dockerServices, Number.MAX_SAFE_INTEGER, 2), { encoding: 'utf8' });
    }
  }, [inputs.outputs, localname, inputs.service.image, inputs.service.ports, inputs.service.environment || []]);

  await useShell({
    command: 'docker-compose',
    args: ['up', '-d'],
    dependsOn: [`${tmpdir}/docker-compose.yml`],
    cwd: tmpdir,
  });

  return inputs.outputs;
});
