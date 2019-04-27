# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from pelican import signals, contents
from bs4 import BeautifulSoup
import asyncio
import os
import json


STREAM_CHUNK_LIMIT = 2 ** 16

class KaTeXRendererManager:
  async def __aenter__(self):
    self.proc = await asyncio.create_subprocess_exec(
      'node',
      os.path.join(os.path.dirname(os.path.abspath(__file__)), 'katex_render.js'),
      stdin=asyncio.subprocess.PIPE,
      stdout=asyncio.subprocess.PIPE,
      limit=STREAM_CHUNK_LIMIT,
      )

    return self.proc

  async def __aexit__(self, exc_type, exc, tb):
    if exc is not None:
      self.proc.terminate()
      return

    await self.proc.wait()

async def katex_math_render_async(content):
  if isinstance(content, contents.Static):
    return

  if not hasattr(content, 'bs4_soup'):
    content.bs4_soup = BeautifulSoup(content._content, 'html.parser')

  soup = content.bs4_soup

  soup_body = soup

  math_block_tags = soup_body.find_all('pre', class_='math')
  math_inline_tags = soup_body.find_all('tt', class_='math')
  if (len(math_block_tags) + len(math_inline_tags)) == 0:
    return

  async with KaTeXRendererManager() as proc:
    async def push_math_content(tag):
      proc.stdin.write(tag.string.replace('\t', '\t ').encode())
      proc.stdin.write(b'\t\n')

    async def readuntil_sep():
      chunk = b''
      while True:
        try:
          chunk = chunk + await proc.stdout.readuntil(b'\t\n')
        except asyncio.LimitOverrunError:
          chunk = chunk + await proc.stdout.read(STREAM_CHUNK_LIMIT - 1024)
          continue
        else:
          break

      return chunk.replace(b'\t ', b'\t')

    async def replace_math_block(tag):
      new_tag = soup_body.new_tag('div')
      new_tag['class'] = 'math block'

      math_html = await readuntil_sep()
      new_tag.append(BeautifulSoup(math_html, 'html.parser').span)

      tag.replace_with(new_tag)

    async def replace_math_inline(tag):
      new_tag = soup_body.new_tag('span')
      new_tag['class'] = 'math inline'

      math_html = await readuntil_sep()
      new_tag.append(BeautifulSoup(math_html, 'html.parser').span)

      tag.replace_with(new_tag)

    if len(math_block_tags) > 0:
      tags = math_block_tags

      await push_math_content(tags[0])
      pre_tag = tags[0]

      for tag in tags[1:None]:
        await asyncio.gather(
          replace_math_block(pre_tag),
          push_math_content(tag),
        )
        pre_tag = tag

      await replace_math_block(pre_tag)

    if len(math_inline_tags) > 0:
      tags = math_inline_tags

      await push_math_content(tags[0])
      pre_tag = tags[0]

      for tag in tags[1:None]:
        await asyncio.gather(
          replace_math_inline(pre_tag),
          push_math_content(tag),
        )
        pre_tag = tag

      await replace_math_inline(pre_tag)

    proc.stdin.write_eof()

  content.bs4_soup = soup
  content._content = soup.decode()

def katex_math_render(content):
  asyncio.run(katex_math_render_async(content))

def register():
  signals.content_object_init.connect(katex_math_render)
