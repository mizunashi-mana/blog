#!/usr/bin/env bash

set -euo pipefail
[ -n "${TRACE:-}" ] && set -x

# devenvとの食い合わせが悪く、共有ライブラリが見つからない問題を避けるため、/usr/bin/lddを使用する
exec env -i "PATH=/usr/bin:$PATH" npx playwright "$@"
