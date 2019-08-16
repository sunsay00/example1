import * as React from 'react';
import * as UI from '@inf/core-ui';
import { Code } from '../components/code';

/*
  p: Paragraph,
  h1: Header1,
  h2: Header2,
  h3: Header3,
  h4: Header4,
  h5: Header5,
  h6: Header6,
  blockquote: Blockquote,
  ul: UnorderedList,
  ol: OrderedList,
  li: ListItem,
  table: Table,
  tr: TableRow,
  td: TableData,
  th: TableHeader,
  code: Code,
  inlineCode: InlineCode,
  hr: HRule,
  a: Anchor,
  tbody: TableBody,
  pre: Preformatted
*/


export default (props: {}) =>
  <>
    <UI.Section>
      <UI.View style={{ marginVertical: 32, marginHorizontal: 16 }}>

        <UI.Header1>Infinage Styleguide</UI.Header1>

        <UI.Header2>Table of Contents</UI.Header2>
        {/*
      - [Components](#components)
      - [Base Tags](#basetags)
      */}

        <UI.Header2>Components</UI.Header2>

        <UI.Header4>Accent</UI.Header4>

        <Code live>{
          `<Accent secondary={false}>Accent</Accent>`
        }</Code>

        <UI.Header4>Badge</UI.Header4>

        <Code live>{
          `<Badge value="13" />`
        }</Code>

        <UI.Header4>BulletCrumbs</UI.Header4>

        <Code live>{
          `<BulletCrumbs crumbs={['one', 'two', ' three']} />`
        }</Code>

        <UI.Header4>BulletCrumbButtons</UI.Header4>

        <Code live>{
          `<BulletCrumbButtons crumbs={['one', 'two', 'three']} />`
        }</Code>

        <UI.Header4>Button</UI.Header4>

        <Code live>{
          `<Button onPress={() => alert('button pressed!')}>Primary Button</Button>`
        }</Code>

        <UI.Header4>Carousel</UI.Header4>

        <Code live>{
          `<Carousel scrollEnabled infinite autoplay>
  <View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'aliceblue'
  }}><Text>Page 1</Text></View>
  <View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'tomato'
  }}><Text>Page 2</Text></View>
  <View style={{
    height: 240, justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: 'rebeccapurple',
  }}><Text>Page 3</Text></View>
</Carousel>`
        }</Code>

        <UI.Header4>Icon</UI.Header4>

        <Code live>{
          `<Icon name="planet" color="tomato" />`
        }</Code>

        <UI.Header4>UserNameInput</UI.Header4>

        <Code live>{
          `<UserNameInput placeholder="User Name" />`
        }</Code>

        <UI.Header4>PasswordInput</UI.Header4>

        <Code live>{
          `<PasswordInput placeholder="Password" message="password required" />`
        }</Code>

        <UI.Header4>EmailInput</UI.Header4>

        <Code live>{
          `<EmailInput placeholder="Email" disabled />`
        }</Code>

        <UI.Header4>NumericInput</UI.Header4>

        <Code live>{
          `<NumericInput placeholder="Age" value={25} />`
        }</Code>

        {/*
      ## Basic Tags <a name="basetags" />
      */}

        <UI.Header1>Header 1</UI.Header1>
        <UI.Header2>Header 2</UI.Header2>
        <UI.Header3>Header 3</UI.Header3>
        <UI.Header4>Header 4</UI.Header4>
        <UI.Header5>Header 5</UI.Header5>
        <UI.Header6>Header 6</UI.Header6>

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
  </>