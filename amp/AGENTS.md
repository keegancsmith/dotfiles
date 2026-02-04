# GitHub

Use the `gh` CLI tool for all GitHub interactions (issues, PRs, repos, etc.). The tool is already authenticated.

# Git Commits

Branch names should be prefixed with `k/` followed by a short descriptive name (e.g., `k/fix-heartbeat-logging`). Do not include my username or full name in branch names—the `k/` prefix is sufficient identification.

## Commit Messages

Write commit messages as prose focusing on the **why** of the change, not the what. The diff already shows what changed—the message should explain the reasoning, context, and motivation behind the change.

Use conventional commit prefixes like `fix/`, `feat/`, `chore/`, `refactor/` followed by the area (e.g., `fix/auth:`, `feat/mcp:`, `chore/dev:`).

Good commit messages:
- Explain the problem being solved
- Provide context on why this approach was chosen
- Reference relevant issues, threads, or documentation when helpful

## Test Plan

Include a `## Test Plan` or `Test Plan:` section in the commit body describing how the change was validated. This can include:
- Manual testing steps taken
- Automated tests added or run
- Observations confirming the fix works

Omit the test plan for changes where there's nothing meaningful to test (e.g., documentation-only changes, typo fixes).

Prefer `Test Plan:` inline unless the commit has multiple sections (e.g., Changelog), then use `## Test Plan`.

## Changelog

Use `## Changelog` sparingly—only for changes worth communicating to end users. Refactors, chores, internal tooling, and similar changes do not need a changelog entry.

# Pull Requests

Use `gh` to create pull requests.

Tag `@sourcegraph/code-plane` for review.

## PR Descriptions

Write PR descriptions as prose, not structured sections like "Summary", "Problem", "Solution". Explain the change naturally—what it does, why it's needed, and how it works—in flowing paragraphs.

Still include `## Test Plan` and `## Changelog` sections as described above for commits.
