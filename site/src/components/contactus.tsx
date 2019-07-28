import * as React from 'react';
import { UI, Breakable } from 'gatsby-theme-core-ui';
import { Headline2 } from './headline';
import { FinePrint } from './fineprint';

export const ContactUs = () =>
  <UI.View style={{ alignItems: 'stretch', marginVertical: 48 }}>
    <UI.View style={{ alignItems: 'center' }}>
      <Headline2 serifed secondary>Let's Talk.</Headline2>
    </UI.View>
    <Breakable
      renderSmall={children =>
        <UI.View style={{ alignItems: 'stretch', marginHorizontal: 32 }}>{children}</UI.View>}
      renderMedium={children =>
        <UI.View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
          <UI.View style={{ maxWidth: '70%' }}>{children}</UI.View>
        </UI.View>}
      renderLarge={children =>
        <UI.View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
          <UI.View style={{ maxWidth: '60%' }}>{children}</UI.View>
        </UI.View>}
    >
      <Breakable
        renderSmall={() =>
          <UI.View style={{ justifyContent: 'space-evenly' }}>
            <UI.TextInput secondary style={{ flex: 1 }} iconType="user" placeholder="First Name *" />
            <UI.TextInput secondary style={{ flex: 1, marginLeft: 22 }} placeholder="Last Name *" />
          </UI.View>}
        renderMedium={() =>
          <UI.View style={{ flexDirection: 'row', justifyContent: 'space-evenly' }}>
            <UI.TextInput secondary style={{ flex: 1 }} iconType="user" placeholder="First Name *" />
            <UI.Spacer />
            <UI.TextInput secondary style={{ flex: 1 }} placeholder="Last Name *" />
          </UI.View>} />
      <UI.EmailInput secondary placeholder="Email Address *" />
      <UI.TextInput secondary iconType="question" multiline placeholder="How can we help?" numberOfLines={5} />
      <FinePrint secondary>Please do not include confidential or sensitive information in your message. In the event that we are representing a party with opposing interests to your own, we may have a duty to disclose any information you provide to our client.</FinePrint>
      <Breakable
        renderSmall={children =>
          <UI.View style={{ alignItems: 'stretch', marginHorizontal: 32 }}>{children}</UI.View>}
        renderMedium={children =>
          <UI.View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
            <UI.View style={{ maxWidth: '70%' }}>{children}</UI.View>
          </UI.View>}
        renderLarge={children =>
          <UI.View style={{ flexDirection: 'row', justifyContent: 'space-around' }}>
            <UI.View style={{ maxWidth: '60%' }}>{children}</UI.View>
          </UI.View>}
      >
        <UI.Spacer />
        <UI.Button secondary disabled>Submit</UI.Button>
      </Breakable>
    </Breakable>
  </UI.View>


