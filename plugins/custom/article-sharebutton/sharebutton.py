# -*- coding: utf-8 -*-

from pelican import signals


def add_sharebutton(article_generator, content):
  print(type(content))

def register():
  signals.article_generator_write_article.connect(add_sharebutton)
