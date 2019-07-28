import * as React from 'react';
import { UI } from 'core-ui';
import { WebNavLayout, WebNavBar, WebNavLink } from 'web-ui';

/*
  p: UI.Paragraph,
  h1: UI.Header1,
  h2: UI.Header2,
  h3: UI.Header3,
  h4: UI.Header4,
  h5: UI.Header5,
  h6: UI.Header6,
  blockquote: UI.Blockquote,
  ul: UI.UnorderedList,
  ol: UI.OrderedList,
  li: UI.ListItem,
  table: UI.Table,
  tr: UI.TableRow,
  td: UI.TableData,
  th: UI.TableHeader,
  code: UI.Code,
  inlineCode: UI.InlineCode,
  hr: UI.HRule,
  a: UI.Anchor,
  tbody: UI.TableBody,
  pre: UI.Preformatted
*/

export default (props: {}) =>
  <WebNavLayout renderNavBar={() =>
    <WebNavBar renderLogo={() => <UI.Image style={{ width: 150 }} resizeMode="contain" source={{ uri: 'logo.png' }} />}>
      <WebNavLink to="services">Services</WebNavLink>
      <WebNavLink to="contact">Contact</WebNavLink>
    </WebNavBar>
  }>
    <UI.Section>
      <UI.View style={{ marginVertical: 32, marginHorizontal: 16 }}>

        <UI.Header1>Infinage Labs</UI.Header1>

        <UI.Header2>Table of Contents</UI.Header2>
        {/*
      - [Components](#components)
      - [Base Tags](#basetags)

## Components <a name="components" />
      */}
        <UI.Header4>Accent</UI.Header4>

        <UI.Code live>{
          `<UI.Accent secondary={false}>Accent</UI.Accent>`
        }</UI.Code>

        <UI.Header4>Badge</UI.Header4>

        <UI.Code live>{
          `<UI.Badge value={13} />`
        }</UI.Code>

        <UI.Header4>BulletCrumbs</UI.Header4>

        <UI.Code live>{
          `< UI.BulletCrumbs crumbs={['one', 'two', ' three']} />`
        }</UI.Code>

        <UI.Header4>BulletCrumbButtons</UI.Header4>

        <UI.Code live>{
          `<UI.BulletCrumbButtons crumbs={['one', 'two', 'three']} />`
        }</UI.Code>

        <UI.Header4>Button</UI.Header4>

        <UI.Code live>{
          `<UI.Button onPress={() => alert('button pressed!')}>Primary Button</UI.Button>`
        }</UI.Code>

        <UI.Header4>Carousel</UI.Header4>

        <UI.Code live>{
          `<UI.Carousel scrollEnabled infinite autoplay>
  <UI.View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'aliceblue'
  }}><UI.Text>Page 1</UI.Text></UI.View>
  <UI.View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'tomato'
  }}><UI.Text>Page 2</UI.Text></UI.View>
  <UI.View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'rebeccapurple',
  }}><UI.Text>Page 3</UI.Text></UI.View>
</UI.Carousel>`
        }</UI.Code>

        <UI.Header4>Icon</UI.Header4>

        <UI.Code live>{
          `<UI.Icon name="stroopwafel" color="tomato" />`
        }</UI.Code>

        <UI.Header4>UserNameInput</UI.Header4>

        <UI.Code live>{
          `<UI.UserNameInput placeholder="User Name" />`
        }</UI.Code>

        <UI.Header4>PasswordInput</UI.Header4>

        <UI.Code live>{
          `<UI.PasswordInput placeholder="Password" message="password required" />`
        }</UI.Code>

        <UI.Header4>EmailInput</UI.Header4>

        <UI.Code live>{
          `<UI.EmailInput placeholder="Email" disabled />`
        }</UI.Code>

        <UI.Header4>NumericInput</UI.Header4>

        <UI.Code live>{
          `<UI.NumericInput placeholder="Age" value={25} />`
        }</UI.Code>

        {/*
      ## Basic Tags <a name="basetags" />
      */}

        <UI.Header1>h1</UI.Header1>

        <UI.Header2>h2</UI.Header2>

        <UI.Header3>h3</UI.Header3>

        <UI.Header4>h4</UI.Header4>

        <UI.Header5>h5</UI.Header5>

        <UI.Header6>h6</UI.Header6>

        {/*

      paragraph1
      paragraph2
      paragraph3

          paragraph4
          paragraph5
          paragraph6

      > blockquote line 1
      > blockquote line 2
      > blockquote line 3
      > blockquote line 4

      - unordered item 1
      - unordered item 2
      - unordered item 3

      1. ordered item A
         - unordered subitem 1
         - unordered subitem 2
      2. ordered item B
      3. ordered item C

      | Col1 | Col2 | Col3    | Col4      |
      | ---- | ---- |:-------:| ---------:|
      | This | is   | a       | table row |
      | This | is   | another | table row |

      `code`

      _em_

      **strong**

      ~~delete~~

      inlineCode `inline code` inlineCode

      ---

<https://infinage.com>

      ![alt](https://static1.squarespace.com/static/59ea4ce9b1ffb6d0755d0fa3/t/5a9dcfc1ec212d191ef4b333/1508695516730/?format=100w)
      */}
      </UI.View>
    </UI.Section>
  </WebNavLayout>