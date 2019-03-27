#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Mizunashi Mana'
SITENAME = '続くといいな日記'
SITEDESCRIPTION = '{AUTHOR}\'s Blog'

SITEURL = 'https://mizunashi-mana.github.io'
ARTICLE_URL = 'posts/{date:%Y}/{date:%m}/{slug}/'
ARTICLE_SAVE_AS = 'posts/{date:%Y}/{date:%m}/{slug}/index.html'
SLUGIFY_SOURCE = 'basename'

THEME = './themes/Flex'
PATH = 'content'
STATIC_PATHS = ['asset']
SITELOGO = '/asset/profile.png'
FAVICON = '/asset/favicon.ico'

BROWSER_COLOR = '#333'
ROBOTS = 'index, follow'

CC_LICENSE = {
    'name': 'Creative Commons Attribution-ShareAlike',
    'version': '4.0',
    'slug': 'by-sa'
}
COPYRIGHT_YEAR = 2019

MAIN_MENU = True

TIMEZONE = 'Asia/Tokyo'

DEFAULT_LANG = 'ja'
OG_LOCALE = 'ja_JP'
LOCALE = 'ja_JP'

# Feed generation is usually not desired when developing
FEED_ALL_RSS = 'feeds/all.rss.xml'
CATEGORY_FEED_RSS = 'feeds/{slug}.rss.xml'
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None
GITHUB_CORNER_URL = 'https://github.com/mizunashi-mana/mizunashi-mana.github.io'

# Blogroll
LINKS = ()

# Social widget
SOCIAL = (
  ('github', 'https://github.com/mizunashi-mana'),
  ('gitlab', 'https://gitlab.com/mizunashi-mana'),
  ('twitter', 'https://twitter.com/Mizunashi_Mana'),
  # ('mastodon', '#'),
  ('rss', '/' + FEED_ALL_RSS),
)

MENUITEMS = (
  ('Archives', '/archives.html'),
  ('Categories', '/categories.html'),
  ('Tags', '/tags.html'),
)

SITEMAP = {
    'format': 'xml',
    'priorities': {
        'articles': 0.6,
        'indexes': 0.6,
        'pages': 0.5,
    },
    'changefreqs': {
        'articles': 'monthly',
        'indexes': 'daily',
        'pages': 'monthly',
    }
}

DEFAULT_PAGINATION = 10

PYGMENTS_RST_OPTIONS = {
  'linenos': 'table',
}

RELATIVE_URLS = True
