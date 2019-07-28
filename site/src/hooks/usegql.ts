import { useRef, useState } from 'react';
import { useQuery as useQueryHook } from 'react-apollo-hooks';
export { useQuery, useMutation, useSubscription } from 'react-apollo-hooks';
import * as GQL from '../graphql';

import { DocumentNode } from 'graphql';
import { has } from 'common';

export const im = {
  update: <Item extends { id: string }>(items: Item[], item: Item, mergeFn?: (a: Item, b: Item) => Item) =>
    items.map(v => v.id == item.id ? (mergeFn ? mergeFn(v, item) : { ...v, ...item }) : v),
  //insert: <Item extends { id: string }>(items: Item[], item: Item) => [item, ...items],
  upsert: <Item extends { id: string }>(items: Item[], item: Item, mergeFn?: (a: Item, b: Item) => Item) =>
    items.find(v => v.id == item.id) ? im.update(items, item, mergeFn) : [item, ...items],
  remove: <Item extends { id: string }>(items: Item[], id: string) => items.filter(v => v.id != id),
  concat: <Item>(items: Item[], newItems: Item[]) =>
    [...items, ...newItems],
  replaceField<K extends keyof Obj, Obj>(objOrObjs: Obj | Obj[], key: K, fn: (...ps: Obj[K][]) => Obj[K]) {
    const objs = objOrObjs instanceof Array ? objOrObjs : [objOrObjs];
    const [fst, ...rst] = objs;
    const o = rst.reduce<Obj>((a, x) => ({ ...a, ...x }), fst);
    return { ...o, [key]: fn.apply(this, objs.map(o => o[key])) };
  }
};

export const usePagedQuery = <T, Key extends keyof GQL.Query, Args extends { cursor?: string, first?: number }, Fields = undefined>(key: Key, doc: DocumentNode, opts?: {
  variables?: Args,
  fetchMoreUpdateQuery?: (previousQueryResult: Query<Key, Fields>, options: {
    fetchMoreResult?: Query<Key, Fields>;
    variables?: Args;
  }) => Query<Key, Fields>;
}) => {
  const result = useQueryHook<Query<Key, Fields>, Args>(doc, {
    variables: opts && opts.variables || undefined,
    fetchPolicy: 'network-only',
  });
  const [fetching, setFetching] = useState(false);
  const lastCursorRef = useRef<string | undefined>(undefined);
  const cursorRef = useRef<string | undefined>(undefined);

  let data: T[] = [];
  if (!result.loading && result.data) {
    const page = result.data[key];
    if (has(page, 'cursor', 'string'))
      cursorRef.current = page.cursor;
    if (has(page, 'items', 'object'))
      if (page.items && page.items instanceof Array)
        data = page.items as T[];
  }

  const fetchMore = () => {
    if (!cursorRef.current) return;
    if (lastCursorRef.current == cursorRef.current) return;
    lastCursorRef.current = cursorRef.current;
    setFetching(true);
    result.fetchMore({
      variables: { cursor: cursorRef.current },
      updateQuery: (prev, options) => {
        if (!opts || !opts.fetchMoreUpdateQuery) return prev;
        setFetching(false);
        return opts.fetchMoreUpdateQuery(prev, options);
      }
    });
  }

  return {
    loading: result.loading,
    error: result.error,
    errors: result.errors,
    data,
    fetching,
    fetchMore,
    refetch: () => result.refetch(),
  };
}