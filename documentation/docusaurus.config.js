// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Bifrost',
  tagline: 'Bifrost is the official Scala-based reference implementation of a full node running the Topl protocol.',
  url: 'https://topl.github.io/',
  baseUrl: '/Bifrost',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'Topl', // Usually your GitHub org/user name.
  projectName: 'Bifrost', // Usually your repo name.

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          lastVersion: 'current',
          versions: {
            current: {
              label: '2.0.0',
              path: 'current',
              badge: true,
            },
          },
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        style: 'dark',
        title: '',
        logo: {
          alt: 'Topl Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'referenceSidebar',
            label: 'Reference',
          },
        //  {
        //    type: 'docSidebar',
        //    sidebarId: 'tutorialsSidebar',
        //    label: 'Tutorials',
        //  },
         {
           type: 'docSidebar',
           sidebarId: 'conceptsSidebar',
           label: 'Concepts',
         },
          {
            href: 'https://github.com/Topl/Bifrost',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Community',
            items: [
              {
                label: 'Blog',
                href: 'https://medium.com/topl-blog',
              },
              {
                label: 'Discord',
                href: 'https://discord.gg/Gp7fFq6Wck',
              },
              {
                label: 'Twitter',
                href: 'https://twitter.com/topl_protocol',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Contribute',
                href: 'https://github.com/Topl/Bifrost',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Topl, Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ['java', 'scala']
      },
    }),
};

module.exports = config;
