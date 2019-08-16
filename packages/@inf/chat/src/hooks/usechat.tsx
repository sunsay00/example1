import * as React from 'react';
import { ChatClient } from '../client';
import { ChatChannel, ChatConnectionState, ChatMessage } from '../types';

const keepTrying = <R extends unknown>(fn: () => Promise<R>, debugName: string): Promise<R> => fn();

type ContextValue = { chat: ChatClient<{}, {}> };

const ChatContext = React.createContext<ContextValue | undefined>(undefined);

export const useChat = (chatIdentity: string) => {
  const chat = React.useContext(ChatContext);
  if (!chat) throw new Error('invalid chat context');

  return chat;
}

export const ChatProvider = (props: { chatIdentity: string, children: React.ReactNode }) => {
  const [chat, setChat] = React.useState<ChatClient<{}, {}> | undefined>();

  React.useEffect(() => {
    const chatClient = new ChatClient<{}, {}>({
      onCurrentChatTokenRequested: async (chatIdentity: string, platform: string) => 'chattoken',
      onCurrentChatIdentityRequested: () => '',
      onChatUserChanged: (params: { identity: string, friendlyName: string | undefined, attributes: { [_: string]: any }, online: boolean, notifiable: boolean }) => { },
      onChannelAdded: (chatIdentity: string, channelDesc: ChatChannel) => { },
      onChannelRemoved: (chatIdentity: string, channelSid: string) => { },
      onChannelUpdated: (chatIdentity: string, channelDesc: ChatChannel) => { },
      onConnectionStateChanged: (connectionState: ChatConnectionState) => { },
      onMessageAdded: (chatIdentity: string, channelSid: string, message: ChatMessage, fromSelf: boolean) => { },
      onUnreadCountChanged: (chatIdentity: string, channelSid: string, index: number) => { },
      onUnreadCountIncremented: (chatIdentity: string, channelSid: string) => { },
      onTypingStarted: (chatIdentity: string, memberSid: string) => { },
      onTypingEnded: (chatIdentity: string, memberSid: string) => { },
    }, props.chatIdentity, keepTrying);

    setChat(undefined);
    chatClient.startup().then(() => {
      setChat(chatClient);
    });
    return () => {
      setChat(undefined);
      chatClient.shutdown();
    }
  }, [props.chatIdentity]);

  if (!chat) return null;

  return (
    <ChatContext.Provider value={{ chat }}>
      {props.children}
    </ChatContext.Provider>
  );
}

