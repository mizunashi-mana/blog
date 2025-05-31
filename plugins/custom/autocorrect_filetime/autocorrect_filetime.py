# -*- coding: utf-8 -*-
import os
from pelican import signals, contents, utils
from datetime import datetime
from .git_process import build_git_process, GitProcess

git_proc = None


def git_process_get() -> GitProcess:
    global git_proc
    if git_proc is None:
        git_proc = build_git_process(os.path.abspath("."))

    return git_proc


def datetime_from_timestamp(timestamp, content):
    return utils.set_date_tzinfo(
        datetime.fromtimestamp(timestamp),
        tz_name=content.settings.get("TIMEZONE", None),
    )


def autocorrect_date_by_filetime(content):
    if isinstance(content, contents.Static):
        return

    if content.metadata.get("gittime", "true") != "true":
        # Disable for this content
        return

    path = content.source_path

    git_proc = git_process_get()

    try:
        committed_times = git_proc.get_committed_times(path)
    except Exception:
        committed_times = {"first_commit_time": None, "last_commit_time": None}

    first_committed_time = committed_times["first_commit_time"]
    last_committed_time = committed_times["last_commit_time"]

    fs_stat = None
    if first_committed_time is not None:
        content.date = first_committed_time
    else:
        if fs_stat is None:
            fs_stat = os.stat(path)
        content.date = datetime_from_timestamp(fs_stat.st_ctime, content)

    if last_committed_time is not None:
        content.modified = last_committed_time
    else:
        if fs_stat is None:
            fs_stat = os.stat(path)
        content.modified = datetime_from_timestamp(fs_stat.st_mtime, content)

    # Clean up content attributes
    if not hasattr(content, "modified"):
        content.modified = content.date

    if hasattr(content, "date"):
        content.locale_date = utils.strftime(content.date, content.date_format)

    if hasattr(content, "modified"):
        content.locale_modified = utils.strftime(content.modified, content.date_format)


def register():
    signals.content_object_init.connect(autocorrect_date_by_filetime)
