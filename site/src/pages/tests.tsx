import * as React from 'react';
import * as UI from '@inf/core-ui';
import { useApollo } from '@inf/apollo';

import { ApolloClient } from 'apollo-client';
import { TryAgain } from '../_gen/tools/tryagain';
import { Paginated } from '../_gen/tools/types';
import { useDecksFindAll, useDecksCreate, useDecksUpdate, useDecksRemove } from '../_gen/models';
import { QueryResult } from '@apollo/react-common';

const Loader = <Vars, Item>(props: {
  variables: Vars
  hooks: (client: ApolloClient<object>, variables: Vars) => QueryResult<Paginated<Item>>
  children: (items: Item[]) => JSX.Element
}) => {
  const client = useApollo();
  const result = props.hooks(client, props.variables);
  if (!result.data) {
    return <UI.Loading />;
  } else if (result.error != undefined) {
    return <TryAgain error={result.error} onRetry={async () => { result.refetch({}) }} />;
  } else {
    return props.children(result.data.items);
  }
}

export default () => {
  const client = useApollo();

  const [createDeck] = useDecksCreate(client);
  const [updateDeck] = useDecksUpdate(client);
  const [removeDeck] = useDecksRemove(client);

  return (
    <Loader variables={{}} hooks={useDecksFindAll}>{items =>
      <>
        <UI.Button onPress={() => createDeck({
          factoryId: '1',
          cards: [],
          expiresAt: new Date(),
          title: 'my title',
          //description: 'my desc',//null as any
        })}>Create</UI.Button>
        <UI.ScrollView>
          {items.map((item, i) => (
            <UI.View key={i} style={{ padding: 8, flexDirection: 'row' }}>
              <UI.Text>{JSON.stringify(item)}</UI.Text>
              <UI.Button size="sm" onPress={() => updateDeck({
                ...item,
                description: item.description == 'wakka' ? 'blakka' : 'wakka',
              })}>Update</UI.Button>
              <UI.Spacer />
              <UI.Button size="sm" onPress={() => removeDeck({ id: item.id })}>Delete</UI.Button>
            </UI.View>))}
        </UI.ScrollView>
      </>
    }</Loader>
  );

  /*if (!result.data) {
    return <UI.Loading />;
  } else if (result.error != undefined) {
    return <TryAgain error={result.error} onRetry={async () => { result.refetch({}) }} />;
  } else {
    return (
      <>
        <UI.Button onPress={() => createDeck({
          factoryId: '1',
          cards: [],
          expiresAt: new Date(),
          title: 'my title',
          //description: 'my desc',//null as any
        })}>Create</UI.Button>
        <UI.ScrollView>
          {result.data.decksFindAll.items.map((item, i) => (
            <UI.View key={i} style={{ padding: 8, flexDirection: 'row' }}>
              <UI.Text>{JSON.stringify(item)}</UI.Text>
              <UI.Button size="sm" onPress={() => updateDeck({
                ...item,
                description: item.description == 'wakka' ? 'blakka' : 'wakka',
              })}>Update</UI.Button>
              <UI.Spacer />
              <UI.Button size="sm" onPress={() => removeDeck({ id: item.id })}>Delete</UI.Button>
            </UI.View>))}
        </UI.ScrollView>
      </>
    );
  }
  */
}
