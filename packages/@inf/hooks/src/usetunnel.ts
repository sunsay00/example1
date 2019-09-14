import { useGlobals } from '@inf/hookops';

export type Tunnel = (props: { command: string, args: string[] }) => { command: string, args: string[] };

export const useTunnel = (inputs: {
  target: { host: string, port: number },
  proxy: { host: string, port: number, }
}) => (cmd: {
  command: string,
  args: string[]
}): { command: string, args: string[] } => {
    const { stage } = useGlobals();
    const host = stage != 'local' ? inputs.target.host : '';
    const proxyhost = stage != 'local' ? inputs.proxy.host : '';
    const localPort = stage != 'local' ? inputs.proxy.port : 0;

    const tunnelargs = stage == 'local' ? ['-s', 'vars'] : ['-s', 'tunnel', `${localPort}`, host, `${inputs.target.port}`, proxyhost];

    return {
      command: 'yarn',
      args: [...tunnelargs, cmd.command, ...(cmd.args || [])],
    };
  }

