import * as React from 'react';
import { sg, View, Text } from 'gatsby-theme-core-ui';

export const Footer = () =>
  <View style={{ alignItems: 'center', marginVertical: 48 }}>
    <Text color={sg.rgba(sg.colors.white, .8)} size="xs" weight="thin">(211) 372-0273 • info@infinage.com</Text>
    <Text color={sg.rgba(sg.colors.white, .8)} size="xs" weight="thin">845 Third Ave., 6th Floor, New York, NY 10022</Text>
  </View>

