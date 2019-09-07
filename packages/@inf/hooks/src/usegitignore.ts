import { useLineInFile } from '@inf/hookops';

export const useGitIgnore = (rootDir: string, filesToIgnore: string[]) =>
  useLineInFile(`${rootDir}/.gitignore`, filesToIgnore);