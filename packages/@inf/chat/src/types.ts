export type ChatChannel = {
  attributes: object,                                     // The Channel's custom attributes
  createdBy: string,                                      // The identity of the User that created this Channel
  dateCreated: Date,                                      // The Date this Channel was created
  dateUpdated: Date,                                      // The Date this Channel was last updated
  friendlyName: string,                                   // The Channel's name
  isPrivate: boolean,                                     // Whether the channel is private (as opposed to public)
  lastConsumedMessageIndex: number | null,                  // Index of the last Message the User has consumed in this Channel
  sid: string,                                            // The Channel's unique system identifier
  status: 'unknown' | 'known' | 'invited' | 'joined' | 'failed',  // Whether the Channel is 'known' to local Client, Client is 'invited' to or is 'joined' to this Channel
  type: 'public' | 'private',                                // The Channel's type as a String: ['private', 'public']
  uniqueName: string,                                     // The Channel's unique name (tag)
};

export type ChatMember = {
  channel: ChatChannel,         // The Channel the remote Client is a Member of
  identity: string,                 // The identity of the remote Client
  isTyping: boolean,                // Whether or not this Member is currently typing
  lastConsumedMessageIndex: number | null, // Latest consumed Message index by this Member
  lastConsumptionTimestamp: Date,   // Date when Member has updated his consumption horizon
  sid: string,                      // The server-assigned unique identifier for the Member
  friendlyName: string,
};

export type ChatMessage = {
  status: 'sending' | 'error' | 'sent',
  author: string,                         // The name of the user that sent Message
  body: string,                           // The body of the Message
  attributes: { [_: string]: any },       // Message custom attributes
  channel: ChatChannel,               // Channel Message belongs to
  dateUpdated: Date,                      // When Message was sent
  index: number,                          // Index of Message in the Channel's messages list
  lastUpdatedBy: string,                  // Identity of the last user that updated Message
  sid: string,                            // The server-assigned unique identifier for Message
  timestamp: Date,                        // When Message was sent
};

export type ChatPushNotification = {
  action: 'opened' | 'background' | 'foreground',
  body: string,
  data: {
    channelSid: string,
    messageIndex: number,
    messageSid: string,
  },
};

export type ChatConnectionState =
  'disconnected' |  // Client is offline and no connection attempt in process.
  'connecting' |    // Client is offline and connection attempt is in process.
  'connected' |     // Client is online and ready.
  'error' |         //	Client connection is in the erroneous state.
  'denied';         // Client connection is denied because of invalid token

export type MessagesPage = {
  hasMore: boolean,
  items: ChatMessage[],
}

export type ChannelsPage = {
  hasMore: boolean,
  items: ChatChannel[],
}

export type ChatClientHandlers = {
  onCurrentChatTokenRequested: (chatIdentity: string, platform: string) => Promise<string>,
  onCurrentChatIdentityRequested: () => string,
  onChatUserChanged: (params: { identity: string, friendlyName: string | undefined, attributes: { [_: string]: any }, online: boolean, notifiable: boolean }) => void,
  onChannelAdded: (chatIdentity: string, channelDesc: ChatChannel) => void,
  onChannelRemoved: (chatIdentity: string, channelSid: string) => void,
  onChannelUpdated: (chatIdentity: string, channelDesc: ChatChannel) => void,
  onConnectionStateChanged: (connectionState: ChatConnectionState) => void,
  onMessageAdded: (chatIdentity: string, channelSid: string, message: ChatMessage, fromSelf: boolean) => void,
  onUnreadCountChanged: (chatIdentity: string, channelSid: string, index: number) => void,
  onUnreadCountIncremented: (chatIdentity: string, channelSid: string) => void,
  onTypingStarted: (chatIdentity: string, memberSid: string) => void,
  onTypingEnded: (chatIdentity: string, memberSid: string) => void,
}
