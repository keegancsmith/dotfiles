#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

mkdir -p agents
{
  cat amp/AGENTS.md
  tail -n +2 amp/GLOBAL.md
} >agents/AGENTS.md
