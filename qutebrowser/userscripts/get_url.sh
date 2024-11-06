#!/usr/bin/env bash

set -e

url=$(echo 'tell application "Google Chrome" to get the URL of front document' | ssh fa.local osascript)

echo "open -t ${url}" >>"${QUTE_FIFO}"
