#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

# This file is only used if you use `make publish` or
# explicitly specify it as your config file.

import os
import sys

sys.path.append(os.curdir)
from pelicanconf import *  # noqa: F403
from pelicanconf import SITELOGO, FAVICON, LINKS, SOCIAL

# If your site is available via HTTPS, make sure SITEURL begins with https://
SITEURL = 'https://mizunashi-mana.github.io/blog'
RELATIVE_URLS = False


def add_site_url(path):
    return SITEURL + path


def add_site_url_to_items(items):
    new_items = []
    for i, item in enumerate(items, start=1):
        link = item['link']
        if link[0] == '/':
            link = add_site_url(link)

        new_items.append({**item, 'link': link})

    return tuple(new_items)


FEED_ALL_ATOM = 'feeds/all.atom.xml'
CATEGORY_FEED_ATOM = 'feeds/{slug}.atom.xml'

DELETE_OUTPUT_DIRECTORY = True

SITELOGO = add_site_url(SITELOGO)
FAVICON = add_site_url(FAVICON)

LINKS = add_site_url_to_items(LINKS)
SOCIAL = add_site_url_to_items(SOCIAL)

GOOGLE_GLOBAL_SITE_TAG = 'G-YZTJG0VBE4'
