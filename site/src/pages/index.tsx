import * as React from 'react';
import * as UI from 'gatsby-theme-core-ui';
import { Cover } from '../components/cover';
import { Headline2 } from '../components/headline';
import { ContactUs } from '../components/contactus';
import { Footer } from '../components/footer';

const Post = (props: { title: string, image?: { uri: string }, children?: React.ReactNode }) =>
  <UI.Breakable
    renderSmall={children => <UI.View style={{ marginBottom: 24, maxWidth: 500 }}>{children}</UI.View>}
    renderMedium={children => <UI.View style={{ marginBottom: 24, maxWidth: 600 }}>{children}</UI.View>}
    renderLarge={children => <UI.View style={{ marginBottom: 24, maxWidth: 700 }}>{children}</UI.View>}>
    {props.image &&
      <UI.View style={{ paddingTop: 16 }}>
        <UI.Image style={{ width: '100%', height: 100, borderRadius: 8 }} source={{ uri: props.image.uri }} resizeMode="cover" />
      </UI.View>}
    <UI.View style={{ flexDirection: 'column', flex: 1 }}>
      <UI.Spacer size="lg" />
      <UI.Header3 serifed>{props.title}</UI.Header3>
      <UI.Text numberOfLines={2} weight="thin" style={{ flexWrap: 'wrap', flex: 1, marginBottom: 8 }}>{props.children}</UI.Text>
    </UI.View>
  </UI.Breakable>

const PostView = (props: { data: { title: string, content: string, image?: { uri: string } }[] }) =>
  <>{props.data.map((post, i) => <Post key={i} title={post.title} image={post.image}>{post.content}</Post>)}</>

const Services = () =>
  <UI.View style={{ margin: 16, alignItems: 'center' }}>
    <Headline2 serifed>Services</Headline2>
    <PostView data={[{
      title: 'Infra√structure Integration',
      content: 'Infinage provides infrastructure integration services, from building new data center to upgrading existing infrastructure. We thoroughly assess any entire IT environment, including desktops, servers, and networks infrastructure to develop necessary improvement plans. Infinage always adapts new technology standards in order to propose the most secure and reliable infrastructure design for our clients’ enterprise network. Because we understand the importance for our clients to maintain a competitive edge at all times, Infinage commits to the delivery of our “least impact” implementation plan for any changes in our clients’ IT infrastructure – that is to keep operational disruption and mechanic resistance, if any, at a minimal level during the integration process, thus streamlining our clients’ current business operations into the novel infrastructure with efficiency. We further assist our clients by minimizing the long-term costs for supporting their infrastructure. In achieving so, Infinage equips our clients’ staffs with technical training and self-help guides to promote internal support and productivity. Our accomplishment is to furnish our clients with a secure, reliable, and manageable support infrastructure.',
      image: { uri: 'network.jpg' }
    }, {
      title: 'Project Management',
      content: 'Infinage recognizes the importance of prudent and conducive management in any project.  We create not only competitive, but also realistic project plans and timelines based on our clients’ requirements for their IT-related work. Infinage assists clients in coordinating and implementing all integration procedures with minimal disruption to their normal business operations. We understand that every resource (time, money, people, materials, energy, space, provision, communication, quality, risk) is critical to our clients; therefore, Infinage IT management is fueled by our desire to optimize allocation and integration of all valuable resources for our clients.',
    }, {
      title: 'Software Development',
      content: 'Software development is the design, creation, and maintenance of functional applications by using the latest IT technologies and practices. Infinage has teams of knowledgeable professionals who can deliver the best software development solutions to any industry.  Throughout the initial stages of software development, Infinage carefully analyzes the scope and ability, or limitations thereof, of our clients’ current applications, so that we can share with confidence our findings and provide necessary information and advice before we start building new applications that better serve our clients’ needs. Based on our analysis, Infinage offers complete software development solutions with user friendly interface, data and usage security coding, database design, and integration plans, which enable our clients to reap the greatest benefits for their businesses.',
      image: { uri: 'codejs.jpg' }

    }, {
      title: 'Onsite Consultant',
      content: 'Infinage is always available to answer any question regarding design, implementation, security, and support with our OnSite IT Consulting Service. At your convenience, our professionals can respond to the most urgent and immediate IT-related issues by way of a personal site visit to best understand and resolve our clients’ concerns. Please feel free to contact us for our competitive service quotation.'
    }, {
      title: 'Marketing and Design',
      content: 'Every corporation strives to increase the market shares for its business. Distinctive and informative marketing materials are unquestionably one of the channels to attain this goal. Infinage entertains this interest for our clients with creative design and marketing.  We conceptualize and author unique designs on all types of marketing materials, such as company logo, corporate website, general graphics, print, brochure, menu, business card, etc. Please feel free to contact us for more information.',
      image: { uri: 'project.jpg' }
    }]} />
  </UI.View>

export default () => {
  return (
    <>
      <UI.Section>
        <Cover />
      </UI.Section>

      <UI.Section>
        <UI.Grid stride={3}>
          <UI.View>
            <UI.View style={{ alignItems: 'center' }}>
              <UI.Icon name="server" color={UI.Colors.accentBlue} />
              <UI.Spacer />
              <UI.Header3 serifed>Experienced IT</UI.Header3>
            </UI.View>
            <UI.Text weight="thin">Infinage specializes in project management, software development, network infrastructure design and implementation, IT security assessment, and IT consulting services.</UI.Text>
          </UI.View>
          <UI.View>
            <UI.View style={{ alignItems: 'center' }} >
              <UI.Icon name="bolt" color={UI.Colors.accentGreen} />
              <UI.Spacer />
              <UI.Header3 serifed>Efficiency driven</UI.Header3>
            </UI.View>
            <UI.Text weight="thin">Our highly trained staffs provide end-to-end IT solutions to our clients from a variety of industries based on their specific needs.</UI.Text>
          </UI.View>
          <UI.View>
            <UI.View style={{ alignItems: 'center' }}>
              <UI.Icon name="globe" color={UI.Colors.accentRed} />
              <UI.Spacer />
              <UI.Header3 serifed>Global solutions</UI.Header3>
            </UI.View>
            <UI.Text weight="thin">We are steadily growing and evolving into a global IT Solutions firm as we continue to fulfill our clients’ escalating exigencies in the international markets.</UI.Text>
          </UI.View>
        </UI.Grid>
      </UI.Section>

      <UI.WebNavAnchor id="services" />

      <UI.View style={{ backgroundColor: UI.rgba(UI.Colors.black, .05), marginHorizontal: -32, paddingHorizontal: 32 }}>
        <UI.Section>
          <Services />
        </UI.Section>
      </UI.View>

      <UI.WebNavAnchor id="contact" />

      <UI.ImageBackground source={{ uri: 'pattern1.png' }} resizeMode="repeat" style={{ marginHorizontal: -32 }}>
        <UI.Section><ContactUs /></UI.Section>
        <UI.Section><Footer /></UI.Section>
      </UI.ImageBackground>
    </>
  );
}