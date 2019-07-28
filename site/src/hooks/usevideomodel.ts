import gql from 'graphql-tag';
import * as GQL from '../graphql';
import { im, usePagedQuery, useMutation, useSubscription } from '../hooks/usegql';
import * as cuid from 'cuid';

const VIDEOS = gql`
query ($first: Int, $cursor: String) {
  videos(first: $first, cursor: $cursor) @connection(key: "videos") {
    items { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
    cursor
  }
}`;

const CREATE_UPLOAD_VIDEO = gql`
mutation ($data: UploadVideoInput!) {
  createUploadVideo(data: $data) { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
}`;

const CREATE_YOUTUBE_VIDEO = gql`
mutation ($data: YouTubeVideoInput!) {
  createYouTubeVideo(data: $data) { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
}`;

const CANCEL_VIDEO = gql`
mutation ($id: ID!) {
  cancelVideo(id: $id) { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
}`;

const VIDEO_ADDED = gql`
subscription onVideoAdded {
  videoAdded { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
}`;

const VIDEO_UPDATED = gql`
subscription onVideoUpdated {
  videoUpdated { id type name url status thumbnailUrl range { start end } resultUrl result logs @client }
}`;

const VIDEO_LOGGED = gql`
subscription onVideoLogged {
  videoLogged { videoId messages }
}`;

export const VIDEO_FRAGMENT = gql`
fragment video on Video {
  id type name url status thumbnailUrl range { start end } resultUrl result logs @client
}`;

type ClientFields = {
  logs: string[]
};

type QueryVideos = Query<'videos', ClientFields>;
type MutationCreateUploadVideo = Mutation<'createUploadVideo', ClientFields>;
type MutationCreateYouTubeVideo = Mutation<'createYouTubeVideo', ClientFields>;
type MutationCancelVideo = Mutation<'cancelVideo', ClientFields>;
type SubscriptionVideoAdded = Subscription<'videoAdded', ClientFields>;
type SubscriptionVideoUpdated = Subscription<'videoUpdated', ClientFields>;
type SubscriptionVideoLogged = Subscription<'videoLogged'>;
type VideoFragment = Fragment<QueryVideos>;

export type ClientVideo = GQL.Video & ClientFields;

const defaultVideo = {
  __typename: 'Video',
  name: '',
  url: '',
  createdAt: new Date().toISOString(),
  status: GQL.VideoStatus.Queued,
  type: GQL.VideoType.Upload,
  thumbnailUrl: '',
  range: {
    __typename: 'Range',
    start: 0,
    end: 0,
  },
  resultUrl: '',
  result: ''
} as const

export const useVideoModel = () => {

  const videos = usePagedQuery<ClientVideo, 'videos', GQL.QueryVideosArgs, ClientFields>('videos', VIDEOS, {
    fetchMoreUpdateQuery: (prev, { fetchMoreResult }) => {
      if (!prev.videos) return prev;
      if (!fetchMoreResult || !fetchMoreResult.videos) return prev;
      return im.replaceField([prev, fetchMoreResult], 'videos', (v, v2) => im.replaceField([v, v2], 'items',
        im.concat));
    }
  });

  const createUploadVideo = useMutation<MutationCreateUploadVideo, GQL.MutationCreateUploadVideoArgs>(CREATE_UPLOAD_VIDEO, {
    optimisticResponse: vars => ({ createUploadVideo: { ...defaultVideo, ...vars.data, id: `-${cuid()}`, logs: [] } }),
    update: (client, { data }) => {
      if (!data) return;
      const prev = client.readQuery<QueryVideos>({ query: VIDEOS });
      if (!prev) return;
      client.writeQuery<QueryVideos>({
        query: VIDEOS,
        data: im.replaceField(prev, 'videos', v => im.replaceField(v, 'items', i =>
          im.upsert(i, data.createUploadVideo)))
      });
    },
  });

  const createYouTubeVideo = useMutation<MutationCreateYouTubeVideo, GQL.MutationCreateYouTubeVideoArgs>(CREATE_YOUTUBE_VIDEO, {
    optimisticResponse: vars => ({ createYouTubeVideo: { ...defaultVideo, ...vars.data, id: `-${cuid()}`, logs: [] } }),
    update: (client, { data }) => {
      if (!data) return;
      const prev = client.readQuery<QueryVideos>({ query: VIDEOS });
      if (!prev) return;
      client.writeQuery<QueryVideos>({
        query: VIDEOS,
        data: im.replaceField(prev, 'videos', v => im.replaceField(v, 'items', i =>
          im.upsert(i, data.createYouTubeVideo)))
      });
    },
  });

  const cancelVideo = useMutation<MutationCancelVideo, GQL.MutationCancelVideoArgs>(CANCEL_VIDEO, {
    optimisticResponse: vars => ({ cancelVideo: { ...defaultVideo, ...vars, status: GQL.VideoStatus.Cancelled, logs: [] } }),
    update: (client, { data }) => {
      if (!data) return;
      const prev = client.readQuery<QueryVideos>({ query: VIDEOS });
      if (!prev) return;
      client.writeQuery<QueryVideos>({
        query: VIDEOS,
        data: im.replaceField(prev, 'videos', v => im.replaceField(v, 'items', i =>
          im.remove(i, data.cancelVideo.id)))
      });
    }
  });

  useSubscription<SubscriptionVideoAdded>(VIDEO_ADDED, {
    onSubscriptionData: ({ client, subscriptionData }) => {
      const { data } = subscriptionData;
      if (!data) return;
      const prev = client.readQuery<QueryVideos>({ query: VIDEOS });
      if (!prev) return;
      client.writeQuery<QueryVideos>({
        query: VIDEOS,
        data: im.replaceField(prev, 'videos', v => im.replaceField(v, 'items', i =>
          im.upsert(i, data.videoAdded, (a, b) => im.replaceField([a, b], 'logs',
            im.concat))))
      });
    }
  });

  useSubscription<SubscriptionVideoUpdated>(VIDEO_UPDATED, {
    onSubscriptionData: ({ client, subscriptionData }) => {
      const { data } = subscriptionData;
      if (!data) return;
      if (data.videoUpdated.status == GQL.VideoStatus.Cancelled) {
        const prev = client.readQuery<QueryVideos>({ query: VIDEOS });
        if (!prev) return;
        client.writeQuery<QueryVideos>({
          query: VIDEOS,
          data: im.replaceField(prev, 'videos', v => im.replaceField(v, 'items', i =>
            im.remove(i, data.videoUpdated.id)))
        });
      } else {
        const prev = client.readFragment<VideoFragment>({
          id: `Video:${data.videoUpdated.id}`,
          fragment: VIDEO_FRAGMENT
        });
        if (!prev) return;
        client.writeFragment<VideoFragment>({
          id: `Video:${data.videoUpdated.id}`,
          fragment: VIDEO_FRAGMENT,
          data: { ...prev, ...data.videoUpdated }
        });
      }
    }
  });

  useSubscription<SubscriptionVideoLogged>(VIDEO_LOGGED, {
    onSubscriptionData: ({ client, subscriptionData }) => {
      const { data } = subscriptionData;
      if (!data) return;
      const prev = client.readFragment<VideoFragment>({
        id: `Video:${data.videoLogged.videoId}`,
        fragment: VIDEO_FRAGMENT
      });
      if (!prev || !prev.logs) return;
      client.writeFragment<VideoFragment>({
        id: `Video:${data.videoLogged.videoId}`,
        fragment: VIDEO_FRAGMENT,
        data: im.replaceField(prev, 'logs', l =>
          im.concat(l, data.videoLogged.messages))
      });
    }
  });

  return { videos, createUploadVideo, createYouTubeVideo, cancelVideo };
}
