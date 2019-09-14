import { rootstate } from '..';

export const useGlobals = () => {
  const config = rootstate.config();
  if (config == undefined)
    throw new Error('root context not initialized');
  if (!config.currentModuleDir)
    throw new Error('useGlobals must be used within a configure context');
  if (config.currentModuleDir == config.hookOpsDir)
    throw new Error('useGlobals must be used within a module context');

  return {
    ...config,
    currentModuleDir: config.currentModuleDir,
    use: config.use
  } as const;
};
