import * as React from 'react';
import { UI } from 'core-ui';

export default (props: { pageContext: { body: string } }) =>
  <UI.View>
    <UI.Text>{props.pageContext.body}</UI.Text>
  </UI.View>