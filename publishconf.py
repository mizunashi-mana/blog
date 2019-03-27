#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

# This file is only used if you use `make publish` or
# explicitly specify it as your config file.

import os
import sys
sys.path.append(os.curdir)
from pelicanconf import *

# If your site is available via HTTPS, make sure SITEURL begins with https://
SITEURL = 'https://mizunashi-mana.github.io/blog'
RELATIVE_URLS = False

def add_site_url(path):
  return SITEURL + path

FEED_ALL_ATOM = 'feeds/all.atom.xml'
CATEGORY_FEED_ATOM = 'feeds/{slug}.atom.xml'

DELETE_OUTPUT_DIRECTORY = True

SITELOGO = add_site_url(SITELOGO)
FAVICON = add_site_url(FAVICON)

for i, item in enumerate(LINKS, start=1):
  if item[0][0] == '/':
    item[0] = add_site_url(item[0])

for i, item in enumerate(SOCIAL, start=1):
  if item[0][0] == '/':
    item[0] = add_site_url(item[0])

for i, item in enumerate(MENUITEMS, start=1):
  if item[0][0] == '/':
    item[0] = add_site_url(item[0])

# Following items are often useful when publishing

#DISQUS_SITENAME = ""
#GOOGLE_ANALYTICS = ""
