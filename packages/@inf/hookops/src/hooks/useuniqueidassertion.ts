import { rootstate } from '..';

const verifyFullyQualifiedUniqueId = (scope: string, value: string) => {
  if (value) {
    if (!/^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?$/.exec(value))
      throw new Error(`invalid id '${value}' - only letters, digits, and dashes are allowed`);
    if (value.includes('---'))
      throw new Error(`invalid id '${value}' - cannot have more than two sequential dashes`);
  }
  const fqid = `${scope}${value ? `:${value}` : ''}`;
  if (!/^[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?(:[a-zA-Z0-9]([a-zA-Z0-9-]*[a-zA-Z0-9])?)?$/.exec(fqid))
    throw new Error(`invalid moduleid '${fqid}' - only letters, digits, and dashes are allowed`);
  if (fqid.includes('---'))
    throw new Error(`invalid moduleid '${fqid}' - cannot have more than two sequential dashes`);
  rootstate.fqIdDeclare(fqid);
  return true;
}

export type UniqueId<T extends string> = { _scope: T, value: string };

export const useUniqueIdAssertion = <Scope extends string>(scope: Scope, value: string) => {
  verifyFullyQualifiedUniqueId(scope, value);
}
