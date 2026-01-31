# Git Commits

Branch names should be prefixed with `k/`.

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

Prefer `Test Plan:` inline unless the commit has multiple sections (e.g., Changelog), then use `## Test Plan`.

## Changelog

Use `## Changelog` sparingly—only for changes worth communicating to end users. Refactors, chores, internal tooling, and similar changes do not need a changelog entry.

# Pull Requests

Use `gh` to create pull requests.

Tag `@sourcegraph/code-plane` for review.
