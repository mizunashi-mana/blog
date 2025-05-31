# -*- coding: utf-8 -*-

from __future__ import unicode_literals

from pelican import signals, contents
from bs4 import BeautifulSoup
import os
import subprocess
import json

KATEX_RENDER_JS_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "katex_render.js"
)
STREAM_CHUNK_LIMIT = 2**16

proc = None


def katex_subprocess_get():
    global proc
    if proc is None:
        proc = subprocess.Popen(
            ["node", KATEX_RENDER_JS_PATH],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )

    return proc


def katex_subprocess_finalize(obj):
    global proc
    if proc is not None:
        try:
            proc.stdin.close()
            proc.wait(timeout=1)
        except subprocess.TimeoutExpired:
            proc.terminate()
        except Exception:
            proc.terminate()

        proc = None


def katex_math_render(content):
    proc = katex_subprocess_get()

    if isinstance(content, contents.Static):
        return

    if not hasattr(content, "bs4_soup"):
        content.bs4_soup = BeautifulSoup(content._content, "html.parser")

    soup = content.bs4_soup

    soup_body = soup

    math_block_tags = soup_body.find_all("pre", class_="math")
    math_inline_tags = soup_body.find_all("tt", class_="math")
    if (len(math_block_tags) + len(math_inline_tags)) == 0:
        return

    def push_math_content(tag, mode: str):
        proc.stdin.write(
            json.dumps(
                {
                    "m": mode,
                    "t": tag.string,
                }
            ).encode("utf-8")
        )
        proc.stdin.write(b"\n")
        proc.stdin.flush()

    def replace_math_content(tag, new_tag):
        chunk = b""
        while not chunk.endswith(b"\n"):
            chunk = chunk + proc.stdout.readline()
            retcode = proc.poll()
            if retcode is not None:
                raise subprocess.CalledProcessError(retcode, proc.args[0])

        rendered_content = json.loads(chunk)
        new_tag.append(
            BeautifulSoup(
                rendered_content["r"],
                "html.parser",
            ).span,
        )

        tag.replace_with(new_tag)

    for tag in math_block_tags:
        push_math_content(tag, "b")

        new_tag = soup_body.new_tag("div")
        new_tag["class"] = "math block"
        replace_math_content(tag, new_tag)

    for tag in math_inline_tags:
        push_math_content(tag, "i")

        new_tag = soup_body.new_tag("span")
        new_tag["class"] = "math inline"
        replace_math_content(tag, new_tag)

    content.bs4_soup = soup
    content._content = soup.decode()


def register():
    signals.finalized.connect(katex_subprocess_finalize)
    signals.content_object_init.connect(katex_math_render)
