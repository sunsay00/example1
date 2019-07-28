import { UI } from 'core-ui';

export const components = {
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
};

export default (props: {}) =>
  <UI.Section>
    <UI.View style={{ marginVertical: 32, marginHorizontal: 16 }}>

      <UI.Header1>Infinage Labs</UI.Header1>

      <UI.Header2>Table of Contents</UI.Header2>
      {/*
      - [Components](#components)
      - [Base Tags](#basetags)

## Components <a name="components" />

      <UI.Header4>Accent</UI.Header4>

      ```jsx live=true
<UI.Accent secondary={false}>Accent</UI.Accent>
      ```

      #### Badge

      ```jsx live=true
<UI.Badge value={13} />
      ```

      #### BulletCrumbs

      ```jsx live=true
<UI.BulletCrumbs crumbs={['one', 'two', ' three']} />
      ```

      #### BulletCrumbButtons

      ```jsx live=true
<UI.BulletCrumbButtons crumbs={['one', 'two', 'three']} />
      ```

      #### Button

      ```jsx live=true
<UI.Button onPress={() => alert('button pressed!')}>Primary Button</UI.Button>
      ```

      #### Carousel

      ```jsx live=true
<UI.Carousel scrollEnabled infinite autoplay>

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

      </UI.Carousel>
      ```

      #### Icon

      ```js live=true
<UI.Icon name="stroopwafel" color="tomato" />
      ```

      #### UserNameInput

      ```js live=true
<UI.UserNameInput placeholder="User Name" />
      ```

      #### PasswordInput

      ```js live=true
<UI.PasswordInput placeholder="Password" message="password required" />
      ```

      #### EmailInput

      ```js live=true
<UI.EmailInput placeholder="Email" disabled />
      ```

      #### NumericInput

      ```js live=true
<UI.NumericInput placeholder="Age" value={25} />
      ```

## Basic Tags <a name="basetags" />

      # h1

      ## h2

      ### h3

      #### h4

      ##### h5

      ###### h6

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