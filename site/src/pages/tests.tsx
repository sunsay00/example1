import * as React from 'react';
import * as UI from '@inf/core-ui';
import { useApollo } from '@inf/apollo';
import { useAccount } from '@inf/cf-cognito';

import { ApolloClient } from 'apollo-client';
import { TryAgain } from '../_gen/tools/tryagain';
import { Paginated } from '../_gen/tools/types';
import {
  useDecksCreate, useDecksUpdate, useDecksRemove, useDecksFindByFactoryId, useDecksFindMine,
  useDeckFactoriesCreate, useDeckFactoriesFindByLocation, useDeckFactoriesRemove
} from '../_gen/models';
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
  //client.resetStore(); client.clearStore();

  const [createDeck] = useDecksCreate(client);
  const [updateDeck] = useDecksUpdate(client);
  const [removeDeck] = useDecksRemove(client);

  const [createDeckFactory] = useDeckFactoriesCreate(client);
  const [removeDeckFactory] = useDeckFactoriesRemove(client);

  const account = useAccount();
  if (!account.user) return null;

  return (
    <UI.Section style={{ paddingVertical: 16 }}>
      {/*<UI.Button secondary onPress={() => createDeck({
        factoryId: '111',
        cards: [],
        expiresAt: new Date(),
        title: 'deck-title',
        description: 'deck-description',
      })}>Create Deck</UI.Button>
      <Loader variables={{ sub: account.user.sub }} hooks={useDecksFindMine}>{deck =>
        <UI.ScrollView>
          {deck.map((item, i) => (
            <UI.View key={i} style={{ padding: 16, flexDirection: 'row', borderColor: UI.Colors.lightGray, borderLeftWidth: i == 0 ? 0 : 1 }}>
              <UI.View style={{ flex: 1, flexDirection: 'column' }}>
                {Object.entries(item).map(([k, v], i) =>
                  <UI.View key={i} style={{ flexDirection: 'row' }}>
                    <UI.Text size="xxs" style={{ maxWidth: 96, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{k}</UI.Text>
                    <UI.Spacer />
                    <UI.Text size="xxs" style={{ maxWidth: 256, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{JSON.stringify(v)}</UI.Text>
                  </UI.View>
                )}
              </UI.View>
              <UI.Spacer />
              <UI.View>
                <UI.Button size="sm" onPress={() => updateDeck({
                  ...item,
                  description: item.description == 'wakka' ? 'blakka' : 'wakka',
                })}>Update</UI.Button>
                <UI.Spacer />
                <UI.Button size="sm" onPress={() => removeDeck({ id: item.id })}>Delete</UI.Button>
              </UI.View>
            </UI.View>))}
        </UI.ScrollView>}
              </Loader>*/}


      <UI.Button onPress={() => createDeckFactory({
        name: 'factory-name',
        description: 'factory-description',
        location: { lat: 35.6683627, lon: 139.6706567 },
      })}>Create DeckFactory</UI.Button>
      <UI.Spacer />
      <Loader variables={{ location: { lat: 35.6683627, lon: 139.6706567 }, radiusMiles: 10 }} hooks={useDeckFactoriesFindByLocation}>{factories =>
        <UI.ScrollView>
          {factories.map((factory, i) => (
            <UI.View key={i}>
              <UI.View style={{ padding: 16, flexDirection: 'row', borderColor: UI.Colors.lightGray, borderTopWidth: i == 0 ? 0 : 1 }}>
                <UI.View style={{ flex: 1, flexDirection: 'column' }}>
                  {Object.entries(factory).map(([k, v], i) =>
                    <UI.View key={i} style={{ flexDirection: 'row' }}>
                      <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{k}</UI.Text>
                      <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{JSON.stringify(v)}</UI.Text>
                    </UI.View>)}
                </UI.View>
                <UI.Button size="sm" onPress={() => removeDeckFactory({ id: factory.id })}>Delete</UI.Button>
              </UI.View>
              <UI.View style={{ padding: 16, flexDirection: 'row' }}>
                <UI.Button secondary onPress={() => createDeck({
                  factoryId: factory.id,
                  cards: [],
                  expiresAt: new Date(),
                  title: 'deck-title',
                  description: 'deck-description',
                })}>Create Deck</UI.Button>
                <Loader variables={{ factoryId: factory.id }} hooks={useDecksFindByFactoryId}>{deck =>
                  <UI.ScrollView horizontal>
                    {deck.map((item, i) => (
                      <UI.View key={i} style={{ padding: 16, flexDirection: 'row', borderColor: UI.Colors.lightGray, borderLeftWidth: i == 0 ? 0 : 1 }}>
                        <UI.View style={{ flex: 1, flexDirection: 'column' }}>
                          {Object.entries(item).map(([k, v], i) =>
                            <UI.View key={i} style={{ flexDirection: 'row' }}>
                              <UI.Text size="xxs" style={{ maxWidth: 96, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{k}</UI.Text>
                              <UI.Spacer />
                              <UI.Text size="xxs" style={{ maxWidth: 256, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{JSON.stringify(v)}</UI.Text>
                            </UI.View>
                          )}
                        </UI.View>
                        <UI.Spacer />
                        <UI.View>
                          <UI.Button size="sm" onPress={() => updateDeck({
                            ...item,
                            description: item.description == 'wakka' ? 'blakka' : 'wakka',
                          })}>Update</UI.Button>
                          <UI.Spacer />
                          <UI.Button size="sm" onPress={() => removeDeck({ id: item.id })}>Delete</UI.Button>
                        </UI.View>
                      </UI.View>))}
                  </UI.ScrollView>}
                </Loader>
              </UI.View>
            </UI.View>))}
        </UI.ScrollView>
      }</Loader>

    </UI.Section>
  );
}
