import * as React from 'react';
import { Section, View, Header1, Header2, Header3, Header4, Header5, Header6 } from 'core-ui';
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
    <Section>
      <View style={{ marginVertical: 32, marginHorizontal: 16 }}>

        <Header1>Infinage Styleguide</Header1>

        <Header2>Table of Contents</Header2>
        {/*
      - [Components](#components)
      - [Base Tags](#basetags)
      */}

        <Header2>Components</Header2>

        <Header4>Accent</Header4>

        <Code live>{
          `<Accent secondary={false}>Accent</Accent>`
        }</Code>

        <Header4>Badge</Header4>

        <Code live>{
          `<Badge value={13} />`
        }</Code>

        <Header4>BulletCrumbs</Header4>

        <Code live>{
          `< BulletCrumbs crumbs={['one', 'two', ' three']} />`
        }</Code>

        <Header4>BulletCrumbButtons</Header4>

        <Code live>{
          `<BulletCrumbButtons crumbs={['one', 'two', 'three']} />`
        }</Code>

        <Header4>Button</Header4>

        <Code live>{
          `<Button onPress={() => alert('button pressed!')}>Primary Button</Button>`
        }</Code>

        <Header4>Carousel</Header4>

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

        <Header4>Icon</Header4>

        <Code live>{
          `<Icon name="stroopwafel" color="tomato" />`
        }</Code>

        <Header4>UserNameInput</Header4>

        <Code live>{
          `<UserNameInput placeholder="User Name" />`
        }</Code>

        <Header4>PasswordInput</Header4>

        <Code live>{
          `<PasswordInput placeholder="Password" message="password required" />`
        }</Code>

        <Header4>EmailInput</Header4>

        <Code live>{
          `<EmailInput placeholder="Email" disabled />`
        }</Code>

        <Header4>NumericInput</Header4>

        <Code live>{
          `<NumericInput placeholder="Age" value={25} />`
        }</Code>

        {/*
      ## Basic Tags <a name="basetags" />
      */}

        <Header1>h1</Header1>

        <Header2>h2</Header2>

        <Header3>h3</Header3>

        <Header4>h4</Header4>

        <Header5>h5</Header5>

        <Header6>h6</Header6>

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
      </View>
    </Section>
  </>