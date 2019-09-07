import { rootstate } from '..';

export const useGlobals = () => {
  const config = rootstate.config();
  if (config == undefined)
    throw new Error('root context not initialized');
  if (config.use == undefined)
    throw new Error('hookops must be used within a configure context');
  if (config.currentRootDir == undefined)
    throw new Error('root context - rootdir not initialized');
  return {
    ...config,
    currentRootDir: config.currentRootDir,
    use: config.use
  };
};
