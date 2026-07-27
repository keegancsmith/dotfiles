# GitHub

Use the `gh` CLI tool for all GitHub interactions (issues, PRs, repos, etc.). The tool is already authenticated.
Do not `curl` GitHub pages or APIs, and do not use generic web fetchers for GitHub URLs; use `gh` subcommands or `gh api` instead.

# Linear

Use the `linear` helper to inspect Linear issues. It takes one issue URL or identifier and dumps the issue, comments, children, and attachments as JSON; for example, `linear CPL-123` or `linear https://linear.app/.../issue/CPL-123/...`.

# Local OSS Checkouts

I often already have open source repositories cloned under `~/src`. Before using remote-repo tools for OSS code, check for a local checkout there first and prefer reading the local copy when it exists.
Examples include `~/src/github.com/bazel-contrib/rules_go` and `~/src/go.googlesource.com/go`.
