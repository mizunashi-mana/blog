#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Mizunashi Mana'
SITENAME = '続くといいな日記'
SITEDESCRIPTION = '{AUTHOR}\'s Blog'.format(AUTHOR=AUTHOR)
SITESUBTITLE = SITENAME

SITEURL = 'localhost:8000'
ARTICLE_URL = 'posts/{date:%Y}/{date:%m}/{slug}/'
ARTICLE_SAVE_AS = 'posts/{date:%Y}/{date:%m}/{slug}/index.html'
MONTH_ARCHIVE_URL = 'posts/{date:%Y}/{date:%m}/'
MONTH_ARCHIVE_SAVE_AS = 'posts/{date:%Y}/{date:%m}/index.html'
YEAR_ARCHIVE_URL = 'posts/{date:%Y}/'
YEAR_ARCHIVE_SAVE_AS =  'posts/{date:%Y}/index.html'

SLUGIFY_SOURCE = 'basename'
PAGE_ORDER_BY = 'basename'
PAGES_SORT_ATTRIBUTE = 'source_path'

THEME = './themes/Flex'
PATH = 'content'
STATIC_PATHS = [
  '.circleci',
  'asset',
]
SITELOGO = '/asset/profile.png'
FAVICON = '/asset/favicon.ico'
CUSTOM_CSS = 'asset/custom.css'

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
GITHUB_URL = 'https://github.com/mizunashi-mana/blog'
GITHUB_CORNER_URL = GITHUB_URL

# Blogroll
LINKS = (
)

# Social widget
SOCIAL = (
  ('envelope', 'mailto:axnnoindexr11@gmail.com'),
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

PLUGIN_PATHS = ['plugins']
PLUGINS = [
  'tipue_search',
  'related_posts',
]

RELATIVE_URLS = True
