# GitHub

Use the `gh` CLI tool for all GitHub interactions (issues, PRs, repos, etc.). The tool is already authenticated.
Do not `curl` GitHub pages or APIs, and do not use generic web fetchers for GitHub URLs; use `gh` subcommands or `gh api` instead.

# Linear

Use the `linear` helper to inspect Linear issues. It takes one issue URL or identifier and dumps the issue, comments, children, and attachments as JSON; for example, `linear CPL-123` or `linear https://linear.app/.../issue/CPL-123/...`.

# Local OSS Checkouts

I often already have open source repositories cloned under `~/src`. Before using remote-repo tools for OSS code, check for a local checkout there first and prefer reading the local copy when it exists.
Examples include `~/src/github.com/bazel-contrib/rules_go` and `~/src/go.googlesource.com/go`.

# Git Commits

Branch names should be prefixed with `k/` followed by a short descriptive name (e.g., `k/fix-heartbeat-logging`). Do not include my username or full name in branch names—the `k/` prefix is sufficient identification.

## Commit Messages

Write commit messages as prose focusing on the **why** of the change, not the what. The diff already shows what changed—the message should explain the reasoning, context, and motivation behind the change.

Use conventional commit prefixes like `fix/`, `feat/`, `chore/`, `refactor/` followed by the area (e.g., `fix/auth:`, `feat/mcp:`, `chore/dev:`).

Good commit messages:
- Explain the problem being solved
- Provide context on why this approach was chosen
- Reference relevant issues, threads, or documentation when helpful

## Changelog

Use `## Changelog` sparingly—only for changes worth communicating to end users. Refactors, chores, internal tooling, and similar changes do not need a changelog entry.

# Pull Requests

Use `gh` to create pull requests.

By default create a draft PR with no reviewers. Only do otherwise if I explicitly ask for it.

## PR Descriptions

Write PR descriptions as prose, not structured sections like "Summary", "Problem", "Solution". Explain the change naturally—what it does, why it's needed, and how it works—in flowing paragraphs.

Use `## Changelog` sections only under the same conditions as commits. Do not add a Test Plan just because a PR template or existing convention might expect one; omit it unless it communicates meaningful validation beyond routine CI.
