import * as React from 'react';
import { ViewStyle, ScrollView } from 'react-native';
import { UI, sg } from 'gatsby-theme-core-ui';
import * as uuid from 'ts-ordered-uuid';
import { useUploadQueue, UploadItem } from '../hooks/useuploadqueue';
import Dropzone from 'react-dropzone';

const ProgressBar = (props: { sofar: number, total: number, disabled?: boolean }) => {
  if (props.sofar > props.total || props.total <= 0 || props.sofar < 0) {
    return (
      <UI.View style={{
        borderWidth: 1, borderColor: sg.colors.accentBlue, height: 8,
        borderRadius: 4, width: '100%', flex: 1,
      }} />);
  } else {
    const pct = (props.sofar / props.total) * 100;
    return (
      <UI.View style={{ flex: 1, marginBottom: -8 }}>
        <UI.View style={{
          backgroundColor: props.disabled ? sg.colors.lightGray : sg.colors.accentBlue, height: 8,
          borderRadius: 4, width: `${pct}%`,
        }} />
        <UI.View style={{
          position: 'relative', top: -8,
          borderWidth: 1, borderColor: props.disabled ? sg.colors.lightGray : sg.colors.accentBlue, height: 8,
          borderRadius: 4, width: '100%',
        }} />
      </UI.View>);
  }
}

const S3UploadItem = (props: { item: UploadItem, limit?: number, onRemove?: () => void, onToggle?: () => void }) =>
  <UI.View style={{
    borderWidth: 1, borderColor: sg.rgba(sg.colors.black, .25), paddingHorizontal: 16,
    paddingTop: 16, paddingBottom: 8, borderRadius: 8, marginBottom: 8
  }}>
    <UI.View style={{
      flexDirection: 'row', alignItems: 'flex-start', justifyContent: 'space-between'
    }}>
      <UI.View style={{ flex: 1, overflow: 'hidden' }}>
        <UI.Header4>{props.item.file.name}</UI.Header4>
      </UI.View>
      <UI.Spacer />
      <UI.View>
        <UI.Text>filesize: {props.item.file.size}</UI.Text>
        <UI.Text>status: {props.item.status}</UI.Text>
      </UI.View>
      <UI.Spacer />
      <UI.Icon size="xs" name="timesCircle" onPress={() => props.onRemove && props.onRemove()} />
    </UI.View>
    {props.limit != undefined && props.item.file.size > props.limit ?
      <UI.View style={{ flexDirection: 'row', alignItems: 'center', justifyContent: 'center' }}>
        <UI.Text>File to large for upload.</UI.Text>
      </UI.View> :
      ((props.item.status == 'active' || props.item.status == 'paused') &&
        <UI.View style={{ flexDirection: 'row', alignItems: 'center' }}>
          <UI.Icon size="xxs" name={props.item.status == 'active' ? 'pause' : 'play'} onPress={() => props.onToggle && props.onToggle()} />
          <UI.Spacer size="sm" />
          <ProgressBar disabled={props.item.status == 'paused'} sofar={props.item.sofar} total={props.item.total} />
        </UI.View>)}
  </UI.View>

export const S3Uploader = (props: {
  style?: ViewStyle,
  multiple?: boolean,
  onUploadComplete?: (name: string, url: string) => void,
}) => {
  const mbLimit = 10;
  const { items, queueItem, removeItem, toggleActiveItem } = useUploadQueue({
    onNameOverride: name => `${uuid.ordered.generate()}.${name.split('.').pop()}`,
    onUploadComplete: (name, url) => props.onUploadComplete && props.onUploadComplete(name, url),
    allowedTypes: ['video/mp4', 'video/mpeg'], mbLimit
  });
  return (
    <UI.View>
      <Dropzone multiple={props.multiple} onDrop={files => props.multiple ? files.forEach(queueItem) : files.length == 1 && queueItem(files[0])}>
        {({ getRootProps, getInputProps }) =>
          <div {...getRootProps()}>
            <input {...getInputProps()} />
            <UI.View style={{
              flex: 1, justifyContent: 'center', alignItems: 'center',
              borderWidth: 2, borderRadius: 8, padding: 32,
              borderStyle: 'dashed', borderColor: sg.rgba(sg.colors.black, .25),
              ...props.style
            }}>
              <UI.Icon size="lg" name="upload" />
              <UI.Spacer />
              <UI.Text style={{ textAlign: 'center' }}>Drop or click here to upload {props.multiple ? 'videos' : 'a video'}.</UI.Text>
              <UI.Spacer size="sm" />
              <UI.Text>(10M limit)</UI.Text>
            </UI.View>
          </div>
        }
      </Dropzone>
      {items.length > 0 &&
        <ScrollView style={{ marginTop: 16 }}>
          {items.map((item, i) =>
            <S3UploadItem
              key={i}
              item={item}
              onRemove={() => removeItem(item)}
              onToggle={() => toggleActiveItem()}
              limit={mbLimit && (1024 * 1024 * mbLimit) || undefined}
            />)}
        </ScrollView>}
    </UI.View>
  );
}