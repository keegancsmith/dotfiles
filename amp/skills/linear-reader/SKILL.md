---
name: linear-reader
description: Reads Linear issues and comments. Use when fetching issue details, listing your assigned issues, or searching for issues in Linear.
---

# Linear Issue Reader

Read issue details and comments from Linear.

## Usage

1. Get issue with `mcp__linear__get_issue` using the issue ID or identifier (e.g., `ENG-123`)
2. Get comments with `mcp__linear__list_comments` using the issue ID

## Always Check Comments

**Always run `mcp__linear__list_comments`** when investigating an issue. Comments often contain critical context including:

- Discussion about the bug or feature
- Proposed solutions or workarounds
- Links to related PRs or issues
- Updates on progress or blockers
