import { useQuery } from '../hooks/usegql';
import { DocumentNode } from 'graphql';
import * as GQL from '../graphql';

export const QueryLoader = <K extends keyof GQL.Query, Args>(props: {
  query: DocumentNode,
  dataKey: K,
  fallback?: JSX.Element,
  children?: (result: { loading: boolean, data?: Query<K, Args>[K] }) => JSX.Element
  variables: Args | undefined
}) => {
  if (!props.variables) return props.fallback || null;
  const result = useQuery<Query<K, Args>>(props.query, { variables: props.variables });
  if (!result) return props.fallback || null;
  if (result.error) {
    console.error(result.error);
    return props.children && props.children({ loading: false, data: undefined }) || null;
    //throw result.error;
  }
  if (result.loading || !result.data) return props.children && props.children({ loading: true, data: undefined }) || null;
  if (!result.data[props.dataKey]) return null;
  return props.children && props.children({ loading: result.loading, data: result.data[props.dataKey] }) || null;
}