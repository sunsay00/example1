import "jest";
import { verifyVars } from '@inf/common';
import { createDefaultResolver } from '../src/legacy/tools/resolver';
import RDSDBClient from '@inf/cf-serverless-postgres';
import { fixtures as fix, IUserContext, Deck } from '@inf/cf-gen';
import { vars } from '@inf/cf-gen/vars';

const OrderedUUID = undefined;//require('ordered-uuid');

const config = verifyVars({
  STAGE: process.env.STAGE,
  AWS_REGION: process.env.AWS_REGION
});

const db = new RDSDBClient(vars.DB_TEST_URL);
const cache = undefined; // new CacheClient(config.redisUrl),

const resolver = createDefaultResolver({
  stage: config.STAGE,
  region: config.AWS_REGION,
  locale: 'en',
  platformApplicationArn: '',
  db,
  cache,
});

describe('decks', () => {
  beforeAll(async () => db.init());
  afterAll(async () => db.deinit());

  const createDeck = async (OrderedUUID: any, sub: string, id: string, factoryId: string, title?: string, description?: string): Promise<Deck> => {
    //rdsstore.now = () => new Date(8888);
    //OrderedUUID.__set(id);
    const createQuery = `mutation ($factoryId: String!, $expiresAt: DateTime, $cards: [DeckCardInput!]!, $title: String = null, $description: String = null) {
      decksCreate (factoryId: $factoryId, expiresAt: $expiresAt, cards: $cards, title: $title, description: $description) {
        sub id createdAt updatedAt factory { id } expiresAt user { name } cards { name value } title description
      }
    }`;
    const user: IUserContext = {
      sub: sub,
      groups: [],
    };
    const createJson = await resolver.resolve({}, createQuery, {
      factoryId,
      expiresAt: new Date(54321).toJSON(),
      cards: [{ name: 'card1', value: 1 }],
      title,
      description
    }, user);
    expect(createJson).toMatchObject({ data: { decksCreate: expect.any(Object) } });
    if (!createJson || !createJson.data) throw new Error('failed to create deck');
    return createJson.data.decksCreate;
  };

  const removeDeck = async (deck: Deck): Promise<Deck> => {
    const removeQuery = `mutation ($id: String!) {
      decksRemove (id: $id) {
        sub id user { name } title description
      }
    }`;
    const removeJson = await resolver.resolve({}, removeQuery, { id: deck.id }, { sub: deck.sub, groups: [] });
    if (removeJson.data == undefined) throw new Error('failed to remove deck');
    return removeJson.data.decksRemove;
  }

  it('should create a default deck', async () => {
    const ctx: IUserContext = { sub: '1', groups: [] };
    const { id, createdAt, updatedAt, expiresAt, ...deck } = await fix.decksCreate(resolver, ctx, '0', [{ name: 'card1', value: 1 }]);
    expect(deck).toMatchSnapshot();
    expect(await fix.decksRemove(resolver, ctx, id)).toBeDefined();
  });

  it('should update a deck', async () => {
    const ctx: IUserContext = { sub: '2', groups: [] };
    const deck1 = await fix.decksCreate(resolver, ctx, '0', [{ name: 'card1', value: 1 }]);
    const deck2 = await fix.decksUpdate(resolver, ctx, deck1.id, [{ name: 'name2', value: 2 }], new Date(77777), 'a different title', 'a different description');
    if (deck2 == undefined) throw new Error('failed to update deck');
    expect(deck2).toBeDefined();
    const { id, createdAt, updatedAt, ...deck } = deck2;
    expect(deck).toMatchSnapshot();
    await fix.decksRemove(resolver, ctx, id);
  });

  it('should deny updating someone else\'s deck', async () => {
    const ctx: IUserContext = { sub: '21', groups: [] };
    const { id: _i1 } = await fix.decksCreate(resolver, ctx, '0', [{ name: 'card1', value: 1 }], new Date(5555), 'a title', 'a description');

    const ctx2: IUserContext = { sub: '22', groups: [] };
    const { id: _i2 } = await fix.decksCreate(resolver, ctx2, '0', [{ name: 'card1', value: 1 }], new Date(4444), 'a title', 'a description');

    const deck1 = await fix.decksUpdate(resolver, ctx, _i2, [{ name: 'name1', value: 1 }], new Date(6666), 'a different title', 'a different desccription');
    expect(deck1).toBeUndefined();

    await fix.decksRemove(resolver, ctx2, _i2);
    await fix.decksRemove(resolver, ctx, _i1);
  });

  it('should remove a deck', async () => {
    const ctx: IUserContext = { sub: 'mysub', groups: [] };
    const { id: i1 } = await fix.decksCreate(resolver, ctx, '0', [], undefined, 'mytitle', 'mydescription');

    const deck1 = await fix.decksRemove(resolver, ctx, i1);
    if (deck1 == undefined) throw new Error('failed to remove deck');
    const { id, createdAt, updatedAt, ...result } = deck1;
    expect(result).toBeDefined();
    expect(result).toMatchSnapshot();

    const found = await fix.decksFindById(resolver, ctx, id);
    expect(found).toBeUndefined();
  });

  it('should fail to remove a deck belonging to another user', async () => {
    const deck = await createDeck(OrderedUUID, '4', 'deck-id-4', '0');
    const removeQuery = `mutation {
      decksRemove (id: "${deck.id}") {
        sub id user { name } title description
      }
    }`;
    const removeJson = await resolver.resolve({}, removeQuery, {}, { sub: '0', username: '0', groups: ['users'] });
    expect(removeJson).toMatchObject({ data: { decksRemove: null } });
    expect(removeJson).toMatchSnapshot();
  });

  it('should get a single deck', async () => {
    const deck = await createDeck(OrderedUUID, '9', 'deck-id-9', '0');
    const deckQuery = `query ($id: String!) {
      decksFindById (id: $id) {
        sub id user { name }
      }
    }`;
    const deckJson = await resolver.resolve({}, deckQuery, { id: deck.id }, { sub: '9', username: '9', groups: ['users'] });
    if (deckJson && deckJson.data && deckJson.data.decksFindById) {
      expect(deckJson.data.decksFindById.id).toEqual(deck.id);
      const { id, ...rest } = deckJson.data.decksFindById;
      deckJson.data.decksFindById = rest;
    }
    expect(deckJson).toMatchSnapshot();
  });

  it('should deny getting someone else\'s single deck', async () => {
    //const mydeck = 
    await createDeck(OrderedUUID, '29', 'deck-id-29', '0');
    const someoneElsesDeck = await createDeck(OrderedUUID, '30', 'deck-id-30', '0');
    const deckQuery = `query ($id: String!) {
      decksFindById (id: $id) {
        sub id user { name }
      }
    }`;
    const deckJson = await resolver.resolve({}, deckQuery, { id: someoneElsesDeck.id }, { sub: '29', username: '9', groups: ['users'] });
    // NOTE: very odd, the deckJson.data object is missing hasOwnProperies, as a workaround serializing and deserializing from json fixes it, look into this later
    expect(JSON.parse(JSON.stringify(deckJson))).toMatchObject({ data: { decksFindById: null } });
    expect(deckJson).toMatchSnapshot();
  });

  it('should get a list of decks', async () => {
    const deck1 = await createDeck(OrderedUUID, '6', 'deck-id-6', '0');
    const deck2 = await createDeck(OrderedUUID, '6', 'deck-id-7', '0');
    const deck3 = await createDeck(OrderedUUID, '7', 'deck-id-8', '0');
    const decksQuery = `query {
      decksFind {
        cursor
        items { sub id user { name } }
      }
    }`;
    const decksJson = await resolver.resolve({}, decksQuery, {}, { sub: '6', username: '6', groups: ['users'] });
    if (!decksJson || !decksJson.data) throw new Error('failed to get decks ' + JSON.stringify(decksJson));
    await removeDeck(deck3);
    await removeDeck(deck2);
    await removeDeck(deck1);
    expect(decksJson.data.decksFind.items).toHaveLength(2);
  });

  it('should get a list of all decks', async () => {
    await createDeck(OrderedUUID, '10', 'deck-id-10', '0');
    await createDeck(OrderedUUID, '10', 'deck-id-11', '0');
    await createDeck(OrderedUUID, '11', 'deck-id-12', '0');
    const decksQuery = `query {
      decksFindAll {
        cursor
        items { sub id user { name } }
      }
    }`;
    const decksJson = await resolver.resolve({}, decksQuery, {}, { sub: '10', username: '10', groups: ['users'] });
    if (!decksJson || !decksJson.data) throw new Error('failed to get all decks ' + JSON.stringify(decksJson));
    expect(decksJson.data.decksFindAll.items.length).toBeGreaterThan(2);
  });

  it('should have a null user if the user does not exist', async () => {
    const ctx: IUserContext = { sub: 'mysub', groups: [] };

    const { id, createdAt, updatedAt, ...deck } = await fix.createDeck(resolver, ctx, '0', 'mytitle', 'mydescription');
    expect(deck.user).toBeNull();
    expect(deck).toMatchSnapshot();

    const result = await fix.removeDeck(resolver, ctx, { id, createdAt, updatedAt, ...deck });
    expect(result).toBeDefined();
  });

  it('should find factories by location', async () => {
    const ctx: IUserContext = { sub: 'factoriesuser1', groups: [] };
    const { id: id1, ...man1 } = await fix.deckFactoriesCreate(resolver, ctx, 'man1', 'mandesc1', { lon: 0.1, lat: 0.1 });
    const { id: id2, ...man2 } = await fix.deckFactoriesCreate(resolver, ctx, 'man2', 'mandesc2', { lon: 0.1, lat: 0.2 });
    const { id: id3, ...man3 } = await fix.deckFactoriesCreate(resolver, ctx, 'man3', 'mandesc3', { lon: 0.1, lat: 0.3 });
    expect(man1).toMatchSnapshot();
    expect(man2).toMatchSnapshot();
    expect(man3).toMatchSnapshot();
    const mans = await fix.deckFactoriesFindByLocation(resolver, ctx, { lon: 0.1, lat: 0.1 }, 10.0);
    expect(mans.items).toHaveLength(2);
    await fix.deckFactoriesRemove(resolver, ctx, id3);
    await fix.deckFactoriesRemove(resolver, ctx, id2);
    await fix.deckFactoriesRemove(resolver, ctx, id1);
  });

});
