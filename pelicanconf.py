#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = 'Mizunashi Mana'
SITENAME = '続くといいな日記'
SITETITLE = SITENAME
SITEDESCRIPTION = '{AUTHOR}\'s Blog'.format(AUTHOR=AUTHOR)

SITEURL = 'http://localhost:8000'
ARTICLE_URL = 'posts/{date:%Y}/{date:%m}/{slug}/'
ARTICLE_SAVE_AS = 'posts/{date:%Y}/{date:%m}/{slug}/index.html'
MONTH_ARCHIVE_URL = 'posts/{date:%Y}/{date:%m}/'
MONTH_ARCHIVE_SAVE_AS = 'posts/{date:%Y}/{date:%m}/index.html'
YEAR_ARCHIVE_URL = 'posts/{date:%Y}/'
YEAR_ARCHIVE_SAVE_AS =  'posts/{date:%Y}/index.html'

SLUGIFY_SOURCE = 'basename'
PAGE_ORDER_BY = 'basename'
PAGES_SORT_ATTRIBUTE = 'source_path'

THEME = 'theme'
PATH = 'content'
STATIC_PATHS = [
    'asset',
    'dist-asset',
    'articles', # mixed content
]
ARTICLE_PATHS = ['articles']
READERS = {
    'html': None
}
SITELOGO = '/asset/profile.png'
FAVICON = '/asset/favicon.ico'

BROWSER_COLOR = '#555'
ROBOTS = 'index, follow'
SUMMARY_MAX_LENGTH = 50

CC_LICENSE = {
    'name': 'コモンズ証 表示-継承 4.0 国際',
    'version': '4.0',
    'slug': 'by-sa',
}
COPYRIGHT_YEAR = 2022

MAIN_MENU = True

TIMEZONE = 'Asia/Tokyo'

DEFAULT_LANG = 'ja'
LOCALE = ['ja_JP.UTF-8', 'ja_JP']
OG_LOCALE = DEFAULT_LANG

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
    ('著者について', 'https://mizunashi-mana.github.io/profile'),
    ('プライバシーポリシー', 'https://mizunashi-mana.github.io/profile/privacy-policy.html'),
)

# Social widget
SOCIAL = (
    ('envelope', 'mailto:contact@mizunashi.work'),
    ('github', 'https://github.com/mizunashi-mana'),
    ('gitlab', 'https://gitlab.com/mizunashi-mana'),
    ('mastodon', 'https://ff.mizunashi.work/@me'),
    ('rss', '/' + FEED_ALL_RSS),
)

SITEMAP = {
    'format': 'xml',
    'priorities': {
        'articles': 0.7,
        'indexes': 0.5,
        'pages': 0.6,
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

PLUGIN_PATHS = [
    'plugins/official',
    'plugins/custom',
]
PLUGINS = [
    'headerid',
    'katex_math_render',
    'autocorrect_filetime',
    'sitemap',
    'related_posts',
]

RELATIVE_URLS = True

DOCUTILS_SETTINGS = {
    'auto_id_prefix': 'auto-id-',
    'math_output': 'LaTeX',
}

HEADERID_LINK_CHAR = '<i class="fas fa-link anchor-link"></i>'
