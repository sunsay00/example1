import * as React from 'react';
import { TextInput, EmailInput, Spacer, Button, View, Breakable } from 'gatsby-theme-core-ui';
import { Headline2 } from './headline';
import { FinePrint } from './fineprint';

export const ContactUs = () =>
  <View style={{ alignItems: 'stretch', marginVertical: 48 }}>
    <View style={{ alignItems: 'center' }}>
      <Headline2 serifed secondary>Let's Talk.</Headline2>
    </View>
    <Breakable
      renderSmall={children =>
        <View style={{ alignItems: 'stretch', marginHorizontal: 32 }}>{children}</View>}
      renderMedium={children =>
        <View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
          <View style={{ maxWidth: '70%' }}>{children}</View>
        </View>}
      renderLarge={children =>
        <View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
          <View style={{ maxWidth: '60%' }}>{children}</View>
        </View>}
    >
      <Breakable
        renderSmall={() =>
          <View style={{ justifyContent: 'space-evenly' }}>
            <TextInput secondary style={{ flex: 1 }} iconType="user" placeholder="First Name *" />
            <TextInput secondary style={{ flex: 1, marginLeft: 22 }} placeholder="Last Name *" />
          </View>}
        renderMedium={() =>
          <View style={{ flexDirection: 'row', justifyContent: 'space-evenly' }}>
            <TextInput secondary style={{ flex: 1 }} iconType="user" placeholder="First Name *" />
            <Spacer />
            <TextInput secondary style={{ flex: 1 }} placeholder="Last Name *" />
          </View>} />
      <EmailInput secondary placeholder="Email Address *" />
      <TextInput secondary iconType="question" multiline placeholder="How can we help?" numberOfLines={5} />
      <FinePrint secondary>Please do not include confidential or sensitive information in your message. In the event that we are representing a party with opposing interests to your own, we may have a duty to disclose any information you provide to our client.</FinePrint>
      <Breakable
        renderSmall={children =>
          <View style={{ alignItems: 'stretch', marginHorizontal: 32 }}>{children}</View>}
        renderMedium={children =>
          <View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
            <View style={{ maxWidth: '70%' }}>{children}</View>
          </View>}
        renderLarge={children =>
          <View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
            <View style={{ maxWidth: '60%' }}>{children}</View>
          </View>}
      >
        <Spacer />
        <Button secondary disabled>Submit</Button>
      </Breakable>
    </Breakable>
  </View>


