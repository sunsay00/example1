import * as React from 'react';
import { ActivityIndicator } from 'react-native';
import { QueryLoader } from './queryloader';
import { UI, sg } from 'gatsby-theme-core-ui';
import * as GQL from '../graphql';
import gql from 'graphql-tag';
import { Debouncer } from 'common';
import { Breakable } from './breakable';

const YTINFO = gql`
query ($videoId: String!) {
  ytInfo (videoId: $videoId) {
    videoId url language title owner channelId thumbnailUrl embedURL datePublished genre paid unlisted
    isFamilyFriendly duration views dislikeCount likeCount channelThumbnailUrl
  }
}`;

const parseVideoId = (url: string | undefined) => {
  if (!url) return undefined;
  const matches = /http.*(v=|\.be\/)([a-zA-Z0-9_-]+)/.exec(url);
  return matches && matches.length == 3 && matches[2] || undefined;
}

const debouncer = new Debouncer();

const formatTime = (secs: number) => {
  const s = Math.round(secs % 59);
  const mins = (secs - s) / 60;
  const m = Math.round(mins % 59);
  const hours = (mins - m) / 60;
  const h = Math.round(hours % 59);
  return `${`${h}`.padStart(2, '0')}:${`${m}`.padStart(2, '0')}:${`${s}`.padStart(2, '0')}`;
}

const Label = (props: { children?: React.ReactNode }) =>
  <UI.Text size="xs" weight="light">{props.children}</UI.Text>

export const YouTubeDetails = (props: {
  url: string | undefined,
  disabled?: boolean,
  onPress?: (name: string, url: string, thumbnailUrl: string, start: number, end: number) => void
}) => {
  const [videoId, setVideoId] = React.useState<string | undefined>(undefined);
  const [offset, setOffset] = React.useState<number>(0);
  React.useEffect(() =>
    debouncer.debounce(setVideoId, 500)(parseVideoId(props.url)),
    [props.url]);
  return (
    <QueryLoader<'ytInfo', GQL.QueryYtInfoArgs> query={YTINFO} dataKey="ytInfo" variables={videoId && { videoId } || undefined}>{({ loading, data }) => {
      if (loading || !data) {
        setOffset(0);
        return <ActivityIndicator />;
      }
      return (
        <UI.View style={{ borderWidth: 1, borderColor: sg.rgba(sg.colors.black, .25), padding: 16, borderRadius: 8 }}>
          <UI.View style={{ flexDirection: 'row', justifyContent: 'space-between' }}>
            <UI.Header4>Video Details</UI.Header4>
            <UI.Button size="sm" disabled={props.disabled} onPress={() => {
              props.onPress && props.onPress(data.title, data.url, data.thumbnailUrl, Math.floor(offset), Math.floor(data.duration));
            }}>Add to Queue</UI.Button>
          </UI.View>
          <Breakable
            renderSmall={children =>
              <UI.View style={{ alignItems: 'stretch' }}>
                <UI.Image style={{ width: '100%', height: 180 }} source={{ uri: data.thumbnailUrl }} />
                {children}
              </UI.View>}
            renderMedium={children =>
              <UI.View style={{ flexDirection: 'row', alignItems: 'stretch' }}>
                <UI.Image style={{ width: 320, height: 180 }} source={{ uri: data.thumbnailUrl }} />
                {children}
              </UI.View>}
          >
            <UI.Spacer size="lg" />
            <UI.View style={{ flex: 1, justifyContent: 'space-between' }}>
              <UI.View>
                <UI.Header6>{data.title}</UI.Header6>
                <Label>Views: {data.views}</Label>
                <Label>LikeCount: {data.likeCount}</Label>
              </UI.View>
            </UI.View>
          </Breakable>
          <UI.View style={{ flexDirection: 'row', alignItems: 'center' }}>
            <Label>Start Time:</Label>
            <UI.Spacer />
            <UI.Slider disabled={props.disabled} minimumTrackTintColor={sg.colors.green} maximumTrackTintColor={sg.rgba(sg.colors.black, .25)} onValueChange={setOffset} minimumValue={0} maximumValue={data.duration} value={offset} />
            <UI.Spacer />
            <Label>{formatTime(offset)}</Label>
          </UI.View>
        </UI.View>);
    }}</QueryLoader>
  );
}
