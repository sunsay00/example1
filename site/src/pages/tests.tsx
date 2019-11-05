import * as React from 'react';
import * as UI from '@infng/core-ui';
import { useApollo, Loader } from '@infng/apollo';
import { useAccount } from '@infng/cf-cognito';
import * as Web from '@infng/gatsby-theme-web-ui';
import { Tabs, Tab } from '../components/tabs';

import {
  useDecksCreate, useDecksUpdate, useDecksRemove, useDecksFindByFactoryId,
  useDeckFactoriesCreate, useDeckFactoriesFindByLocation, useDeckFactoriesRemove
} from '../_gen/models';

const DeckFactoryTests = () => {

  const client = useApollo();

  const [name, setName] = React.useState('');

  const [createDeck] = useDecksCreate(client);
  const [updateDeck] = useDecksUpdate(client);
  const [removeDeck] = useDecksRemove(client);

  const [createDeckFactory] = useDeckFactoriesCreate(client);
  const [removeDeckFactory] = useDeckFactoriesRemove(client);

  const account = useAccount();
  if (!account.user) return null;

  return (
    <Loader variables={{
      location: { lat: 35.6683627, lon: 139.6706567 },
      radiusMiles: 10,
      count: 5
    }} hooks={useDeckFactoriesFindByLocation}>{(factories, { refetch, fetchMore, fetching }) => {
      return (
        <>
          <UI.View style={{ flexDirection: 'row' }}>
            <UI.TextInput placeholder="Name" onChangeText={setName} value={name} />
            <UI.Spacer />
            <UI.Button disabled={!name} onPress={() => {
              createDeckFactory({
                name,
                description: 'factory-description',
                location: { lat: 35.6683627, lon: 139.6706567 },
              });
              setName('');
            }}>Create DeckFactory</UI.Button>
            <UI.Spacer />
            <UI.Button secondary onPress={refetch}>Refetch</UI.Button>
          </UI.View>
          <UI.Spacer />
          <Web.EndReachedDetector onEndReached={fetchMore} onEndReachedThreshold={0.5}>
            <UI.ScrollView>
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
                              {Object.entries(item).filter(([k]) => ['factory', 'user', 'cards', 'title', 'description'].includes(k)).map(([k, v], i) =>
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
                                description: item.description == 'test1' ? 'test2' : 'test1',
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
          </Web.EndReachedDetector>
        </>);
    }}</Loader>
  );
}

import {
  useDeviceListsMyDevices, useDeviceListsRegister, useDeviceListsUnregister
} from '../_gen/models';
const DeviceListTests = () => {

  const chatIdentity = 'chatId1';

  const [token, setToken] = React.useState('1');

  const client = useApollo();

  const [register] = useDeviceListsRegister(client);
  const [unregister] = useDeviceListsUnregister(client);

  const account = useAccount();
  if (!account.user) return null;

  return (
    <Loader variables={{
      chatIdentity,
    }} hooks={useDeviceListsMyDevices}>{(devices, { refetch, fetchMore, fetching }) => {
      return (
        <>
          <UI.Header5>ChatIdentity: {chatIdentity}</UI.Header5>
          <UI.View style={{ flexDirection: 'row', alignItems: 'center' }}>
            <UI.TextInput placeholder="Token" onChangeText={setToken} value={token} />
            <UI.Spacer />
            <UI.Button disabled={!token} onPress={async () => {
              await register({ token, chatIdentity, userAgent: 'web' });
            }}>Register New Device</UI.Button>
            <UI.Spacer />
            <UI.Button secondary onPress={refetch}>Refetch</UI.Button>
          </UI.View>
          <UI.Spacer />
          <Web.EndReachedDetector onEndReached={fetchMore} onEndReachedThreshold={0.5}>
            <UI.ScrollView>
              {devices.map((device, i) => (
                <UI.View key={i}>
                  <UI.View style={{ padding: 16, flexDirection: 'row', borderColor: UI.Colors.lightGray, borderTopWidth: i == 0 ? 0 : 1 }}>
                    <UI.View style={{ flex: 1, flexDirection: 'column' }}>
                      {Object.entries(device).map(([k, v], i) =>
                        <UI.View key={i} style={{ flexDirection: 'row' }}>
                          <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{k}</UI.Text>
                          <UI.Text size="xxs" style={{ flex: 1, flexWrap: 'wrap' }}>{JSON.stringify(v)}</UI.Text>
                        </UI.View>)}
                    </UI.View>
                    <UI.Button disabled={!chatIdentity} size="sm" onPress={() => unregister({ chatIdentity, token: device.token })}>Unregister Device</UI.Button>
                  </UI.View>
                </UI.View>))}
            </UI.ScrollView>
          </Web.EndReachedDetector>
        </>);
    }}</Loader>
  );
}
export default () => {

  return (
    <UI.Section style={{ paddingVertical: 16 }}>
      <Tabs initalIndex={1}>
        <Tab title="DeckFactory">
          <DeckFactoryTests />
        </Tab>
        <Tab title="DeviceList">
          <DeviceListTests />
        </Tab>
      </Tabs>
    </UI.Section>
  );
}
