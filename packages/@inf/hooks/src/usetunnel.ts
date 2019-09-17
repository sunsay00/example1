import { useGlobals } from '@inf/hookops';

export type TunnelProps = {
  target: { host: string, port: number },
  proxy: { host: string, port: number, }
}

export const useTunnel = (props: TunnelProps) => (cmd: {
  command: string,
  args: string[]
}): { command: string, args: string[] } => {
  const { stage } = useGlobals();
  const host = stage != 'local' ? props.target.host : '';
  const proxyhost = stage != 'local' ? props.proxy.host : '';
  const localPort = stage != 'local' ? props.proxy.port : 0;

  const tunnelargs = stage == 'local' ? ['-s', 'vars'] : ['-s', 'tunnel', `${localPort}`, host, `${props.target.port}`, proxyhost];

  return {
    command: 'yarn',
    args: [...tunnelargs, cmd.command, ...(cmd.args || [])],
  };
}

