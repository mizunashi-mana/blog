# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from pelican import signals, contents
from bs4 import BeautifulSoup
import textwrap


def embed_customjs(content):
  if isinstance(content, contents.Static):
    return

  if not hasattr(content, 'bs4_soup'):
    content.bs4_soup = BeautifulSoup(content._content, 'html.parser')

  soup = content.bs4_soup

  if isinstance(content, contents.Article):
    content_type = 'articles'
  elif isinstance(content, contents.Page):
    content_type = 'pages'
  else:
    content_type = 'unknown'

  soup_body = soup

  source_path = content.get_relative_source_path()

  embed_metadata_script = soup_body.new_tag('script')
  embed_metadata_script.string = textwrap.dedent("""
  var contentMetadataForCustomJS = {{
    'type': '{content_type}',
    'title': document.title,
    'url': location.protocol + '//' + location.host + location.pathname,
    'source_path': '{source_path}',
    'github_url': '{github_url}'
  }}
  """.format(
    content_type= content_type,
    source_path= source_path,
    github_url= content.settings.get('GITHUB_URL') + '/blob/master/content/' + source_path,
  ))
  soup_body.append(embed_metadata_script)

  jsfile_script = soup_body.new_tag('script')
  jsfile_script['src'] = content.settings.get('JS_FILE')
  jsfile_script['async'] = None
  soup_body.append(jsfile_script)

  content.bs4_soup = soup
  content._content = soup.decode()

def register():
  signals.content_object_init.connect(embed_customjs)
