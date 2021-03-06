import * as React from 'react';
import * as UI from '@infng/core-ui';
import { useApollo, Loader } from '@infng/apollo';
import { useAccount } from '@infng/cf-cognito';
import { useNavOptions, useNav, NavHeaderButton } from '../hooks/usenav';

export const Profile = () => {
  const nav = useNav();

  const account = useAccount();

  useNavOptions({
    title: 'Profile',
    headerLeft: <NavHeaderButton prefixIconName="menu" onPress={nav.openDrawer} />,
    headerRight: <NavHeaderButton onPress={account.logOut}>Log out</NavHeaderButton>
  });

  const client = useApollo();

  /*
  const [createDeck] = useDecksCreate(client);
  const [updateDeck] = useDecksUpdate(client);
  const [removeDeck] = useDecksRemove(client);

  const [createDeckFactory] = useDeckFactoriesCreate(client);
  const [removeDeckFactory] = useDeckFactoriesRemove(client);

  if (!account.user) return <UI.Loading />;

  return (
    <>
      <Loader variables={{
        location: { lat: 35.6683627, lon: 139.6706567 },
        radiusMiles: 10,
        count: 5
      }} hooks={useDeckFactoriesFindByLocation}>{(factories, { refetch, fetchMore, fetching }) =>
        <UI.View style={{ flex: 1 }}>
          <UI.View style={{ flexDirection: 'row', padding: 16 }}>
            <UI.Button onPress={() => createDeckFactory({
              name: 'factory-name',
              description: 'factory-description',
              location: { lat: 35.6683627, lon: 139.6706567 },
            })}>Create DeckFactory</UI.Button>
            <UI.Spacer />
            <UI.Button secondary onPress={refetch}>Refetch</UI.Button>
          </UI.View>
          <UI.Spacer />
          <UI.ScrollView style={{ flex: 1 }}>
            {factories.map((factory, i) => (
              <UI.View key={i}>
                <UI.View style={{ padding: 16, flexDirection: 'row', borderColor: UI.Colors.lightGray, borderTopWidth: i == 0 ? 0 : 1 }}>
                  <UI.View style={{ flex: 1, flexDirection: 'column' }}>
                    {Object.entries(factory).filter(([k]) => ['name', 'description', 'location'].includes(k)).map(([k, v], i) =>
                      <UI.View key={i} style={{ flexDirection: 'row' }}>
                        <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{k}</UI.Text>
                        <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{JSON.stringify(v)}</UI.Text>
                      </UI.View>)}
                  </UI.View>
                  <UI.Button size="sm" onPress={() => removeDeckFactory({ id: factory.id })}>Delete</UI.Button>
                </UI.View>
                <UI.View style={{ padding: 16, flexDirection: 'row' }}>
                  <UI.Button size="sm" secondary onPress={() => createDeck({
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
                            {Object.entries(item).filter(([k]) => ['user', 'cards', 'title', 'description'].includes(k)).map(([k, v], i) =>
                              <UI.View key={i} style={{ flexDirection: 'row' }}>
                                <UI.Text size="xxs" style={{ maxWidth: 96, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{k}</UI.Text>
                                <UI.Spacer />
                                <UI.Text size="xxs" style={{ maxWidth: 256, flex: 1, flexWrap: 'wrap', overflow: 'hidden' }}>{JSON.stringify(v)}</UI.Text>
                              </UI.View>
                            )}
                          </UI.View>
                          <UI.Spacer />
                          <UI.View>
                            <UI.Button size="xs" onPress={() => updateDeck({
                              ...item,
                              description: item.description == 'wakka' ? 'blakka' : 'wakka',
                            })}>Update</UI.Button>
                            <UI.Spacer />
                            <UI.Button size="xs" onPress={() => removeDeck({ id: item.id })}>Delete</UI.Button>
                          </UI.View>
                        </UI.View>))}
                    </UI.ScrollView>}
                  </Loader>
                </UI.View>
              </UI.View>))}
          </UI.ScrollView>
        </UI.View>}
      </Loader>
    </>
  );
  */
  return null;
}
