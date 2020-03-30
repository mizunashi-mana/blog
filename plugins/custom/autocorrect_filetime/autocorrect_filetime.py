import os
from datetime import datetime
from pelican import signals, contents, utils

def datetime_from_timestamp(timestamp, content):
    return utils.set_date_tzinfo(
        datetime.fromtimestamp(timestamp),
        tz_name=content.settings.get('TIMEZONE', None))

def autocorrect_date_by_filetime(content):
  if isinstance(content, contents.Static):
    return

  path = content.source_path
  fs_creation_time = datetime_from_timestamp(os.stat(path).st_ctime, content)

  if not hasattr(content, 'date'):
    content.date = fs_creation_time
    content.locale_date = utils.strftime(content.date, content.date_format)

def register():
  signals.content_object_init.connect(autocorrect_date_by_filetime)
