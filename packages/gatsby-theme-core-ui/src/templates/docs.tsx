import * as React from 'react';
import { View, Text } from 'core-ui';

export default (props: { pageContext: { body: string } }) =>
  <View>
    <Text>{props.pageContext.body}</Text>
  </View>