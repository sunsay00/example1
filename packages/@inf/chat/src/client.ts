import { AccessManager as TwilioAccessManager } from 'twilio-common';
import { Channel as TwilioChannel } from 'twilio-chat/lib/channel';
import { ChannelDescriptor as TwilioChannelDescriptor } from 'twilio-chat/lib/channeldescriptor';
import { Client as TwilioChatClient } from 'twilio-chat';
import { Member as TwilioMember } from 'twilio-chat/lib/member';
import { Message as TwilioMessage } from 'twilio-chat/lib/message';
import { Paginator as TwilioPaginator } from 'twilio-chat/lib/interfaces/paginator';
import { Platform } from 'react-native';
import { User as TwilioUser } from 'twilio-chat/lib/user';
import { ChatClientHandlers, ChannelsPage, MessagesPage, ChatMember, ChatMessage, ChatChannel, ChatPushNotification, ChatConnectionState } from './types';

const MESSAGES_PER_PAGE = 30;

type OnKeepTrying = <R>(fn: () => Promise<R>, debugName: string) => Promise<R>;

type Result<T> = { __result: boolean; err?: any; ok: T; }

class _Err extends Error {
  constructor(m: string, ...prev: any[]) {
    super(prev != undefined ? `${m}\n${prev.join('\n')}` : m);
    console.error(m);
  }
}
const ERROR = (...args: any[]): _Err => new _Err(args.join(' '));
function ASSERT<T>(c: Result<T> | boolean, ...msgs: any[]): void {
  if (typeof c == 'boolean') {
    if (!c) {
      console.error(`ASSERTION-FAILED: ${msgs.join(' - ')}`);
      //Sentry.nativeCrash();
      throw new Error(`ASSERTION-FAILED: ${msgs.join(' - ')}`);
    }
  } else {
    if (c.err) {
      console.error(`ASSERTION-FAILED: ${c.err} - ${msgs.join(' - ')}`);
      //Sentry.nativeCrash();
      throw new Error(`ASSERTION-FAILED: ${c.err} - ${msgs.join(' - ')}`);
    }
  }
}
const ok = <T>(v: T): Result<T> => ({ __result: true, ok: v });
const er = <T>(v: any): Result<T> => ({ __result: false, ok: null as any, err: v });
const fn = <R>(f: () => void | Promise<R>): Promise<Result<R>> => new Promise<Result<R>>(async (y, n) => {
  try {
    const x = f();
    if (typeof x == 'object') { y(ok(await x)); }
  } catch (err) {
    //if (err instanceof _Err) { throw err; } else
    y(er(err));
  }
});
const to = function <T>(promise: T | Promise<T>): Result<T> | Promise<Result<T>> {
  if (promise && (promise as any).then != undefined) {
    return (promise as Promise<T>).then(k => ok(k)).catch(err => er(err));
  } else {
    try {
      return ok(promise as T);
    } catch (err) {
      return er(err);
    }
  }
}

class ChatCache {
  _channels = { bySid: {} as { [_: string]: TwilioChannel | undefined }, byUniqueName: {} as { [_: string]: TwilioChannel | undefined } };
  getChannelBySid = async (chat: TwilioChatClient, channelSid: string) => {
    let ret = this._channels.bySid[channelSid];
    if (ret) return ret;
    ret = await chat.getChannelBySid(channelSid);
    this.addChannel(ret);
    return ret;
  }
  getChannelByUniqueName = async (chat: TwilioChatClient, uniqueName: string) => {
    let ret = this._channels.bySid[uniqueName];
    if (ret) return ret;
    ret = await chat.getChannelByUniqueName(uniqueName);
    this.addChannel(ret);
    return ret;
  }
  addChannel = (channel: TwilioChannel) => {
    this._channels.bySid[channel.sid] = channel;
    this._channels.byUniqueName[channel.uniqueName] = channel;
  }
  removeChannel = (channel: TwilioChannel) => {
    delete this._channels.bySid[channel.sid];
    delete this._channels.byUniqueName[channel.uniqueName];
  }

  _users = { byIdentity: {} as { [_: string]: TwilioUser | undefined } }
  getUser = async (chat: TwilioChatClient, identity: string) => {
    let ret = this._users.byIdentity[identity];
    if (ret) return ret;
    ret = await chat.getUser(identity);
    this._users.byIdentity[ret.identity] = ret;
    return ret;
  }
}


export const messageToMessage = (message: TwilioMessage): ChatMessage => {
  return {
    status: 'sent',
    author: message.author,
    body: message.body,
    attributes: message.attributes,
    channel: channelToDesc(message.channel),
    dateUpdated: message.dateUpdated,
    index: message.index,
    lastUpdatedBy: message.lastUpdatedBy,
    sid: message.sid,
    timestamp: message.timestamp,
  };
};

export const channelToDesc = (channel: TwilioChannel): ChatChannel => {
  return {
    attributes: channel.attributes,
    createdBy: channel.createdBy,
    dateCreated: channel.dateCreated,
    dateUpdated: channel.dateUpdated,
    friendlyName: channel.friendlyName,
    isPrivate: channel.isPrivate,
    lastConsumedMessageIndex: channel.lastConsumedMessageIndex,
    sid: channel.sid,
    status: channel.status,
    type: channel.type,
    uniqueName: channel.uniqueName,
  };
}

export const descriptorToDesc = (desc: TwilioChannelDescriptor): ChatChannel => {
  return {
    attributes: desc.attributes,
    createdBy: desc.createdBy,
    dateCreated: desc.dateCreated,
    dateUpdated: desc.dateUpdated,
    friendlyName: desc.friendlyName,
    isPrivate: desc.isPrivate,
    lastConsumedMessageIndex: desc.lastConsumedMessageIndex,
    sid: desc.sid,
    status: desc.status,
    type: desc.type,
    uniqueName: desc.uniqueName,
  };
};

export class ChatClient<UserAttributes, ChannelAttributes> {
  private _chatIdentity: string;
  private _chat?: TwilioChatClient;
  private _onPushNotification?: (notificiation: ChatPushNotification) => void = undefined;
  private _shuttingdown: boolean;
  private _cache = new ChatCache();
  private _handlers: ChatClientHandlers;
  private _keepTrying: OnKeepTrying;

  constructor(handlers: ChatClientHandlers, chatIdentity: string, keepTrying: OnKeepTrying) {
    this._shuttingdown = false;
    this._chatIdentity = chatIdentity;
    this._handlers = handlers;
    this._keepTrying = keepTrying;
  }

  startup = async () => {
    if (this._chat != undefined) throw ERROR('invalid chat client');
    const chat = await this.createClient(this._chatIdentity);
    ASSERT(chat, 'failed to connect to chat');
    this._chat = chat.ok;
  }

  wake = async () => {
    if (this._chat != undefined) {
      const x = this._chat as any;
      if (x && x.options && x.options.twilsockClient && x.options.twilsockClient._socket) {
        const sock = x.options.twilsockClient._socket;
        if (sock.reconnect && sock.isRetrying) {
          const retrying = sock.isRetrying();
          if (retrying) {
            console.log('CHAT RECONNECT');
            return sock.reconnect();
          }
        }
      }
    }
  }

  chatIdentity = () => {
    return this._chatIdentity;
  }

  shutdown = async () => {
    const chat = this._chat;
    if (chat == undefined) throw ERROR('invalid chat client');
    this._shuttingdown = true;
    chat.removeAllListeners();
    await this._keepTrying(() => chat.shutdown(), 'ChatClient.shutdown');
    this._cache = new ChatCache();
    this._chat = undefined;
  }

  isConnected = () => {
    if (this._chat == undefined) return false;
    return this._chat.connectionState == 'connected';
  }

  private createClient = (chatIdentity: string) => fn(() =>
    new Promise<TwilioChatClient>(async (resolve, reject) => {
      const token = await this._keepTrying<string>(async () => this._handlers.onCurrentChatTokenRequested(chatIdentity, Platform.OS), 'ChatClient.getToken');
      const client = await this._keepTrying(() => TwilioChatClient.create(token, {
        //logLevel: 'trace',
      }), 'ChatClient.createClient');

      const accessManager = new TwilioAccessManager(token);
      accessManager.on('tokenUpdated', am => fn(async () => {
        //const ret = await to(client.ok.updateToken(am.token));
        //if (ret.err) console.error(ret.err);
        await this._keepTrying(async () => client.updateToken(am.token), 'ChatClient.createClient.tokenUpdated');
      }));
      accessManager.on('tokenExpired', () => {
        setTimeout(async () => {
          try {
            if (!this._shuttingdown) {
              const token = await this._keepTrying<string>(async () => this._handlers.onCurrentChatTokenRequested(chatIdentity, Platform.OS), 'ChatClient.getToken (2)');
              accessManager.updateToken(token);
            }
          } catch (err) {
            console.error(`CHAT TOKEN EXPIRED ERROR ${err}`);
          }
        }, 200); // twilio spams this call if a bad token is given, slow it down
      });

      client.on('userSubscribed', (obj: TwilioUser) => {
        //console.warn('userSubscribed');
      });
      client.on('userUpdated', (user: TwilioUser) => fn(() => {
        this._handlers.onChatUserChanged({
          identity: user.identity,
          friendlyName: user.friendlyName == null ? undefined : user.friendlyName,
          attributes: user.attributes,
          online: user.online,
          notifiable: user.notifiable,
        });
      }));
      client.on('userUnsubscribed', (obj: object) => {
        console.warn('userUnsubscribed');
      });
      client.on('channelAdded', async (channel: TwilioChannel) => {
        this._cache.addChannel(channel);
        //const messageCount = await channel.getMessagesCount(); // optional
        const unconsumed = await channel.getUnconsumedMessagesCount();
        const index = unconsumed == null ? 0 : unconsumed;
        //console.log('CHANNEL_ADDED', 'UN:', unconsumed, 'IDX:', index, 'LCMI:', channel.lastConsumedMessageIndex, 'TTL:', messageCount);
        this._handlers.onChannelAdded(chatIdentity, channelToDesc(channel));
        this._handlers.onUnreadCountChanged(chatIdentity, channel.sid, index);
      });
      client.on('channelRemoved', (channel: TwilioChannel) => {
        this._handlers.onChannelRemoved(chatIdentity, channel.sid);
        this._cache.removeChannel(channel);
      });
      client.on('channelInvited', async (channel: TwilioChannel) => {
        const ret = await to(this._keepTrying(() => channel.join(), 'ChatClient.channelInvited'));
        if (ret.err) console.error(ret.err);
      });
      client.on('channelJoined', (obj: TwilioChannel) => {
        //console.warn('channelJoined');
      });
      client.on('channelLeft', (channel: TwilioChannel) => {
        //console.warn('channelLeft');
      });
      client.on('channelUpdated', (channel: TwilioChannel) => {
        this._handlers.onChannelUpdated(chatIdentity, channelToDesc(channel));
      });
      client.on('memberJoined', (obj: TwilioMember) => {
        //console.warn('memberJoined');
      });
      client.on('memberLeft', (obj: TwilioMember) => {
        //console.warn('memberLeft');
      });
      client.on('memberUpdated', (obj: TwilioMember) => {
        //console.warn('memberUpdated');
      });

      let _lastConnectionStateWorkaround = undefined as ChatConnectionState | undefined;
      client.on('connectionStateChanged', async (connectionState: ChatConnectionState) => {

        // WORKAROUND: current version of twilio flickers between connecting/connected states even
        // though the connection seems valid, using a timer to detect and ignore this false positive
        if (connectionState != 'connecting' || _lastConnectionStateWorkaround != 'connected')
          this._handlers.onConnectionStateChanged(connectionState)
        _lastConnectionStateWorkaround = undefined;
        setTimeout(() => _lastConnectionStateWorkaround = connectionState, 1000);

        if (connectionState == 'connected') {
          resolve(client);
        } else if (connectionState == 'disconnected') {
          client.removeAllListeners();
          setTimeout(async () => {
            const ret = await this.createClient(chatIdentity);
            if (ret.err) console.error('failed to reconnect to chat');
          }, 5000);
        }
      });
      client.on('messageAdded', (message: TwilioMessage) => {
        console.log('MESSAGE-ADDED', message.body);
        const fromSelf = this._handlers.onCurrentChatIdentityRequested() == message.author;
        this._handlers.onMessageAdded(chatIdentity, message.channel.sid, messageToMessage(message), fromSelf);
        !fromSelf && this._handlers.onUnreadCountIncremented(chatIdentity, message.channel.sid);
      });
      client.on('messageUpdated', (obj: TwilioMessage) => {
        console.warn('messageUpdated');
      });
      client.on('messageRemoved', (obj: TwilioMessage) => {
        console.warn('messageRemoved');
      });
      client.on('typingStarted', (member: TwilioMember) => {
        this._handlers.onTypingStarted(chatIdentity, member.sid);
      });
      client.on('typingEnded', (member: TwilioMember) => {
        this._handlers.onTypingEnded(chatIdentity, member.sid);
      });
      client.on('pushNotification', (notification: ChatPushNotification) => {
        this._onPushNotification && this._onPushNotification(notification);
        //if (notification.action == 'background') {
        //Root.dispatch(Root.actions()._incrementUnreadCounts(chatIdentity, notification.data.channelSid));
        //}
      });
    })
  );

  handlePushNotification = async (rawNotification: object) => {
    if (this._chat == undefined) {
      console.warn('invalid chat client');
    } else {
      this._chat.handlePushNotification(rawNotification);
    }
  }

  setDeviceToken = async (deviceToken: string) => {
    if (this._chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channelType = Platform.select({ ios: 'apn', android: 'fcm' }) as 'apn' | 'fcm';
      this._chat.setPushRegistrationId(channelType, deviceToken);
    }
  }

  setOnPushNotification = (onPushNoitification: (notification: ChatPushNotification) => void) => {
    this._onPushNotification = onPushNoitification;
  }

  setAllMessagesConsumed = async (channelSid: string, index: number) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.setAllMessagesConsumed');
      await this._keepTrying(async () => channel.setAllMessagesConsumed(), 'ChatClient.setAllMessagesConsumed (2)');
    }
  }

  channelLeave = async (channelSid: string) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.channelLeave');
      const members = await this._keepTrying(() => channel.getMembers(), 'ChatClient.channelLeave (2)');
      if (members.findIndex(m => m.identity == this._chatIdentity) != -1) {
        await this._keepTrying(() => channel.delete(), 'ChatClient.channelLeave (3)');;
      }
    }
  }

  createChannelFetcher = () => {
    return (() => {
      let paginator: TwilioPaginator<TwilioChannelDescriptor> | undefined;
      return async (): Promise<Result<ChannelsPage>> => fn(async () => {
        const chat = this._chat;
        if (chat == undefined) {
          console.warn('invalid chat client');
          return {
            hasMore: false,
            items: [] as ChatChannel[],
          };
        } else if (paginator == undefined) {
          const paginatorresult = await this._keepTrying(() => chat.getUserChannelDescriptors(), 'ChatClient.createChannelFetcher');
          paginator = paginatorresult;
          if (paginator.items == undefined || paginator.items.length == undefined) {
            console.error('invalid chat channel fetcher paginator (1)', paginator.items);
          }
          return {
            hasMore: paginator.hasNextPage,
            items: paginator.items.map(descriptorToDesc) as ChatChannel[],
          };
        } else if (paginator.hasPrevPage) {
          const p = paginator;
          const paginatorresult = await this._keepTrying(() => p.prevPage(), 'ChatClient.createChannelFetcher (2)');
          paginator = paginatorresult;
          if (paginator.items == undefined || paginator.items.length == undefined) {
            console.error('invalid chat channel fetcher paginator (2)', paginator.items);
          }
          return {
            hasMore: paginator.hasNextPage,
            items: paginator.items.map(descriptorToDesc) as ChatChannel[],
          };
        } else {
          return {
            hasMore: false,
            items: [] as ChatChannel[],
          };
        }
      });
    })();
  }

  userExists = async () => {
    const chat = this._chat;
    if (chat == undefined || this._chatIdentity == undefined) {
      console.warn('invalid chat client');
      return false;
    } else {
      const user = await this._keepTrying(() => this._cache.getUser(chat, this._chatIdentity), 'ChatClient.userExists');
      return user != undefined;
    }
  }

  getChannelByUniqueName = async (uniqueName: string) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
      return undefined;
    } else {
      const ret = await this._keepTrying(async () => {
        try {
          return await this._cache.getChannelByUniqueName(chat, uniqueName);
        } catch (err) {
          // make sure not to keep retrying 404s, twilio uses this as an indication that the channel does not exist (IMHO this should not be an exception)
          if (err.status == 404) {
            return undefined;
          } else {
            throw err;
          }
        }
      }, 'ChatClient.getChannelByUniqueName');
      if (ret == undefined) {
        return undefined;
      } else {
        return channelToDesc(ret);
      }
    }
  }

  getChannelBySid = async (channelSid: string) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
      return undefined;
    } else {
      const ret = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.getChannelBySid');
      return channelToDesc(ret);
    }
  }

  createChannel = async (uniqueName: string, friendlyName: string) => {
    const chat = this._chat;
    if (chat == undefined) throw ERROR('invalid chat client');
    const channel = await this._keepTrying(() => chat.createChannel({ uniqueName, friendlyName, isPrivate: true }), `ChatClient.createChannel: ${uniqueName} ${friendlyName}`);
    await this._keepTrying(() => channel.join(), 'ChatClient.createChannel (2)');
    return channelToDesc(channel);
  }

  channelAddMember = async (desc: ChatChannel, memberIdentityId: string) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, desc.sid), 'ChatClient.createAddMember');
      const members = await this._keepTrying(() => channel.getMembers(), 'ChatClient.channelAddMember (2)');
      if (members.findIndex(m => m.identity == memberIdentityId) == -1) {
        await this._keepTrying(() => channel.add(memberIdentityId), 'ChatClient.createAddMember (3)');
      }
    }
  }

  createMessageFetcher = (channelSid: string, anchor?: number) => {
    return (() => {
      let paginator: TwilioPaginator<TwilioMessage> | undefined = undefined;
      return async (): Promise<MessagesPage> => {
        const chat = this._chat;
        if (chat == undefined) {
          console.warn('invalid chat client');
          return {
            hasMore: false,
            items: [] as ChatMessage[],
          };
        } else if (paginator == undefined) {
          const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.createMessageFetcher');
          const presult = await this._keepTrying(async () => channel.getMessages(MESSAGES_PER_PAGE), 'ChatClient.createMessageFetcher (2)');
          paginator = presult;
          return {
            hasMore: paginator.hasPrevPage,
            items: paginator.items.map(messageToMessage) as ChatMessage[],
          };
        } else if (paginator.hasPrevPage) {
          const p = paginator;
          const presult = await this._keepTrying(() => p.prevPage(), 'ChatClient.createMessageFetcher (3)');
          paginator = presult;
          return {
            hasMore: paginator.hasPrevPage,
            items: paginator.items.map(messageToMessage) as ChatMessage[],
          };
        } else {
          return {
            hasMore: false,
            items: [] as ChatMessage[],
          };
        }
      };
    })();
  }

  memberToMember = async (member: TwilioMember): Promise<ChatMember> => {
    return {
      channel: channelToDesc(member.channel),
      identity: member.identity,
      isTyping: member.isTyping,
      lastConsumedMessageIndex: member.lastConsumedMessageIndex,
      lastConsumptionTimestamp: member.lastConsumptionTimestamp,
      sid: member.sid,
      friendlyName: (await this._keepTrying(async () => member.getUserDescriptor(), 'ChatClient.memberToMember')).friendlyName,
    };
  }

  fetchMembers = async (channelSid: string): Promise<ChatMember[]> => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
      return [];
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.fetchMembers');
      const members = await this._keepTrying(async () => channel.getMembers(), 'ChatClient.fetchMembers (2)');
      const ret = [] as ChatMember[];
      for (let i = 0; i < members.length; ++i) {
        ret.push(await this.memberToMember(members[i]));
      }
      return ret;
    }
  }

  channelTyping = async (channelSid: string) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.channelTyping');
      channel.typing();
    }
  }

  channelUpdateAttributes = async (channelSid: string, attributes: Partial<ChannelAttributes>) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.channelUpdateAttributes');
      channel.updateAttributes(attributes as object);
    }
  }

  channelSendMessage = async (channelSid: string, message: string, uuid?: string): Promise<string | undefined> => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('failed to send message - invalid chat client');
      return undefined;
    } else {
      //if (!Root.isOnline()) {
      //console.warn('failed to send message - not online');
      //return undefined;
      //}
      const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.channelSendMessage');
      const ret = await this._keepTrying(() => channel.sendMessage(message, uuid == undefined ? undefined : { uuid }), 'ChatClient.channelSendMessage');
      return ret;
    }
  }

  userUpdateAttributes = async (attributes: Partial<UserAttributes>) => {
    const chat = this._chat;
    if (chat == undefined) {
      console.warn('invalid chat client');
    } else {
      await this._keepTrying(() => chat.user.updateAttributes(attributes), 'ChatClient.userUpdateAttributes');
    }
  }

  getUser = async (identity: string): Promise<TwilioUser | undefined> => {
    const chat = this._chat;
    if (chat == undefined) throw ERROR('invalid chat client');
    const user = await this._keepTrying(async () => {
      try {
        return await this._cache.getUser(chat, identity);
      } catch (err) {
        if (err.status == 404)
          return undefined;
        else
          throw err;
      }
    }, 'ChatClient.getUser');
    return user;
  }

  setUserFriendlyName = async (identity: string, friendlyName: string) => {
    const chat = this._chat;
    if (chat == undefined) throw new Error('invalid chat client');
    const user = await this._keepTrying(() => this._cache.getUser(chat, identity), 'ChatClient.setUserFriendlyName');
    await this._keepTrying(() => user.updateFriendlyName(friendlyName), 'ChatClient.setUserFriendlyName (2)');
  }

  getChannelInfo = async (channelSid: string) => {
    const chat = this._chat;
    if (chat == undefined) throw ERROR('invalid chat client');
    const channel = await this._keepTrying(async () => this._cache.getChannelBySid(chat, channelSid), 'ChatClient.getChannelInfo');
    const members = await this._keepTrying(() => channel.getMembers(), 'ChatClient.getChannelInfo');
    return `${channel.uniqueName} ${members.map(m => m.identity)}`;
  }
}