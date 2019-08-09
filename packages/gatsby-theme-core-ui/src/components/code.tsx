import * as React from 'react';
//import { TextStyle } from 'react-native';
import { LiveProvider, LiveEditor, LiveError, LivePreview } from 'react-live';
import Highlight, { Language, PrismTheme, defaultProps } from 'prism-react-renderer';
import * as UI from 'core-ui';

const lightTheme: PrismTheme = {
  plain: { backgroundColor: undefined/*"#faf8f5"*/, color: "#728fcb" },
  styles: [{ types: ["comment", "prolog", "doctype", "cdata", "punctuation"], style: { color: "#b6ad9a" } },
  { types: ["namespace"], style: { opacity: 0.7 } },
  { types: ["tag", "operator", "number"], style: { color: "#063289" } },
  { types: ["property", "function"], style: { color: "#b29762" } },
  { types: ["tag-id", "selector", "atrule-id"], style: { color: "#2d2006" } },
  { types: ["attr-name"], style: { color: "#896724" } },
  { types: ["boolean", "string", "entity", "url", "attr-value", "keyword", "control", "directive", "unit", "statement", "regex", "at-rule"], style: { color: "#728fcb" } },
  { types: ["placeholder", "variable"], style: { color: "#93abdc" } },
  { types: ["deleted"], style: { textDecorationLine: "line-through" } },
  { types: ["inserted"], style: { textDecorationLine: "underline" } },
  { types: ["italic"], style: { fontStyle: "italic" } },
  { types: ["important", "bold"], style: { fontWeight: "bold" } },
  { types: ["important"], style: { color: "#896724" } }]
};

const darkTheme: PrismTheme = {
  plain: { color: "#F8F8F2", backgroundColor: "#282A36" },
  styles: [{ types: ["prolog", "constant", "builtin"], style: { color: "rgb(189, 147, 249)" } },
  { types: ["inserted", "function"], style: { color: "rgb(80, 250, 123)" } },
  { types: ["deleted"], style: { color: "rgb(255, 85, 85)" } },
  { types: ["changed"], style: { color: "rgb(255, 184, 108)" } },
  { types: ["punctuation", "symbol"], style: { color: "rgb(248, 248, 242)" } },
  { types: ["string", "char", "tag", "selector"], style: { color: "rgb(255, 121, 198)" } },
  { types: ["keyword", "variable"], style: { color: "rgb(189, 147, 249)", fontStyle: "italic" } },
  { types: ["comment"], style: { color: "rgb(98, 114, 164)" } },
  { types: ["attr-name"], style: { color: "rgb(241, 250, 140)" } }]
};

export const Code = (props: { className?: string, style?: UI.TextStyle, children: string, live?: boolean, secondary?: boolean }) => {
  const language = (props.className && props.className.replace(/language-/, '') || 'jsx') as Language;
  const theme = props.secondary ? lightTheme : darkTheme;
  if (props.live) {
    return (
      <LiveProvider code={props.children} theme={theme} language={language} scope={{
        Accent: UI.Accent,
        Badge: UI.Badge,
        BulletCrumbs: UI.BulletCrumbs,
        BulletCrumbButtons: UI.BulletCrumbButtons,
        Button: UI.Button,
        Carousel: UI.Carousel,
        Icon: UI.Icon,
        UserNameInput: UI.UserNameInput,
        PasswordInput: UI.PasswordInput,
        EmailInput: UI.EmailInput,
        NumericInput: UI.NumericInput,
        Header1: UI.Header1,
        Header2: UI.Header2,
        Header3: UI.Header3,
        Header4: UI.Header4,
        Header5: UI.Header5,
        Header6: UI.Header6,
        Text: UI.Text,
        ScrollView: UI.ScrollView,
        View: UI.View,
      }}>
        <UI.Breakable style={{ marginBottom: 16 }}
          renderSmall={children =>
            <UI.View style={{ flex: 1, alignItems: 'stretch', justifyContent: 'flex-start' }}>{children}</UI.View>}
          renderMedium={children =>
            <UI.View style={{ flex: 1, flexDirection: 'row', alignItems: 'stretch', justifyContent: 'space-evenly' }}>{children}</UI.View>}>
          <UI.View style={{ flex: 1, marginBottom: 16 }}>
            <LiveEditor style={{ minHeight: 100, height: '100%' }} />
            <LiveError />
          </UI.View>
          <UI.View style={{ flex: 1, justifyContent: 'center', paddingLeft: 16 }}>
            <LivePreview style={{ minHeight: 100, height: '100%' }} />
          </UI.View>
        </UI.Breakable>
      </LiveProvider>
    );
  } else {
    return (
      <Highlight {...defaultProps} theme={theme} code={props.children} language={language}>
        {({ style, tokens, getLineProps, getTokenProps }) => (
          <pre style={{ ...style, borderRadius: 4, overflow: 'auto', padding: 12, margin: 0, fontSize: 13 }}>
            {tokens.map((line, i) => (
              <div key={i} {...getLineProps({ line, key: i })}>
                {line.filter(l => !l.empty).map((token, key) => <span key={key} {...getTokenProps({ token, key })} />)}
              </div>
            ))}
          </pre>
        )}
      </Highlight>
    );
  }
}

export const InlineCode = (props: { style?: UI.TextStyle, children: string, secondary?: boolean }) =>
  <Highlight {...defaultProps} theme={props.secondary ? lightTheme : darkTheme} code={props.children} language={'jsx'}>
    {({ style, tokens, getLineProps, getTokenProps }) => (
      <pre style={{ ...style, borderRadius: 4, paddingLeft: 4, paddingRight: 4, margin: 0, fontSize: 13 }}>
        {tokens.map((line, i) => (
          <div key={i} {...getLineProps({ line, key: i })}>
            {line.filter(l => !l.empty).map((token, key) => <span key={key} {...getTokenProps({ token, key })} />)}
          </div>
        ))}
      </pre>
    )}
  </Highlight>