#!/usr/bin/env bash

set -e

url=$(echo 'tell application "Safari" to get the URL of front document' | ssh real osascript)

echo "open -t ${url}" >>"${QUTE_FIFO}"
