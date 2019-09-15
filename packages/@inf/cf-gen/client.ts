import { useQuery, useMutation } from '@apollo/react-hooks';
import { DocumentNode } from 'graphql';
import { ApolloClient } from 'apollo-client';
import { Paginated } from './types';

/*
export const useFindAllQuery = <Vars, Item, FindAllKey extends string>(client: ApolloClient<object>, findAllKey: FindAllKey, findAllQuery: DocumentNode, variables?: Vars) => {
  const result = useQuery(findAllQuery, {
    client,
    variables,
    fetchPolicy: 'network-only'
  });
  return {
    loading: result.loading,
    error: result.error,
    data: result.data && result.data[findAllKey] as { [_ in FindAllKey]: Item }[FindAllKey] | undefined
  };
};

export const useRemoveMutation = <
  Vars,
  Item extends { id: string },
  RemoveKey extends string,
  FindAllKey extends string
>(client: ApolloClient<object>, removeKey: RemoveKey, removeQuery: DocumentNode, findAllKey: FindAllKey, findAllQuery: DocumentNode, _: Vars) => {
  const [mutate, __] = useMutation<{ [_ in RemoveKey]: Item }>(removeQuery, {
    client,
    update: (proxy, { data }) => {
      if (!data || !data[removeKey]) return;
      const result: Item = data[removeKey];
      try {
        const dataFindAll = proxy.readQuery<{ [_ in FindAllKey]: Paginated<Item> }>({ query: findAllQuery });
        if (dataFindAll) {
          dataFindAll[findAllKey].items = dataFindAll[findAllKey].items.filter(i => i.id != result.id);
          proxy.writeQuery({ query: findAllQuery, data: dataFindAll });
        }
      } catch (err) {
        console.log('cache not updated (remove-mutation)', err);
      }
    }
  });
  return (variables: Vars) => mutate({ variables });
}

export const useUpdateMutation = <
  Vars,
  Item extends { id: string },
  UpdateKey extends string,
  FindAllKey extends string
>(client: ApolloClient<object>, updateKey: UpdateKey, updateQuery: DocumentNode, findAllKey: FindAllKey, findAllQuery: DocumentNode, _: Vars) => {
  const [mutate, __] = useMutation<{ [_ in UpdateKey]: Item }>(updateQuery, {
    client,
    update: (proxy, { data }) => {
      if (!data || !data[updateKey]) return;
      const result = data[updateKey];
      try {
        const dataFindAll = proxy.readQuery<{ [_ in FindAllKey]: Paginated<Item> }>({ query: findAllQuery });
        if (dataFindAll) {
          dataFindAll[findAllKey].items.map(item => item.id != result.id ? item : { ...item, ...result });
          proxy.writeQuery({ query: findAllQuery, data: dataFindAll });
        }
      } catch (err) {
        console.log('cache not updated (update-mutation)', err);
      }
    },
  });
  return (variables: Vars) => mutate({ variables });
}

export const useCreateMutation = <
  Vars,
  Item extends {},
  CreateKey extends string,
  FindAllKey extends string
>(client: ApolloClient<object>, createKey: CreateKey, createQuery: DocumentNode, findAllKey: FindAllKey, findAllQuery: DocumentNode, _: Vars) => {
  const [mutate, __] = useMutation<{ [_ in CreateKey]: Item }, Vars>(createQuery, {
    client,
    update: (proxy, { data }) => {
      if (!data) return;
      const result = data[createKey];
      try {
        const dataFindAll = proxy.readQuery<{ [_ in FindAllKey]: Paginated<Item> }>({ query: findAllQuery });
        if (dataFindAll) {
          dataFindAll[findAllKey].items.unshift(result);
          proxy.writeQuery({ query: findAllQuery, data: dataFindAll });
        }
      } catch (err) {
        console.log('cache not updated (create-mutation)', err);
      }
    }
  });
  return (variables: Vars) => mutate({ variables });
}

const useDecksFindAll = (client: ApolloClient<object>, variables?: DecksFindAllProps): QueryResult<DecksFindAllResult> => {
  return useFindAllQuery(client, 'decksFindAll', decksFindAllQuery, variables);
}
const useRemoveDeckMutation = (client: ApolloClient<object>) => useRemoveMutation(client, 'decksRemove', decksRemoveQuery, 'decksFindAll', decksFindAllQuery, {} as DecksRemoveProps);
const useUpdateDeckMutation = (client: ApolloClient<object>) => useUpdateMutation(client, 'decksUpdate', decksUpdateQuery, 'decksFindAll', decksFindAllQuery, {} as DecksUpdateProps);
const useCreateDeckMutation = (client: ApolloClient<object>) => useCreateMutation(client, 'decksCreate', decksCreateQuery, 'decksFindAll', decksFindAllQuery, {} as DecksCreateProps);

type QueryResult<R> = {
  loading: boolean,
  error: ApolloError | undefined
  data: R | undefined,
}

*/