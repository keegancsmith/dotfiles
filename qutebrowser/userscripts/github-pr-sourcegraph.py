#!/usr/bin/env python3

"""Open the current GitHub PR's branch in internal Sourcegraph compare."""

import os
import re
import subprocess
from pathlib import Path
from urllib.parse import quote, urlparse


SOURCEGRAPH = "https://sourcegraph.sourcegraph.com"


def pr_from_url(url):
    parsed = urlparse(url)
    if parsed.hostname != "github.com":
        return None

    match = re.match(r"^/([^/]+)/([^/]+)/pull/(\d+)(?:/|$)", parsed.path)
    return match.groups() if match else None


def qute_command(command):
    with Path(os.environ["QUTE_FIFO"]).open("a") as fifo:
        fifo.write(command + "\n")


def error(message):
    message = message.replace("\\", "\\\\").replace("'", "\\'").replace("\n", " ")
    qute_command(f"message-error '{message}'")


def main():
    if "QUTE_FIFO" not in os.environ:
        raise SystemExit("This script must be run by qutebrowser.")

    pr = pr_from_url(os.environ.get("QUTE_URL", ""))
    if pr is None:
        error("This command only works on GitHub pull request pages.")
        return

    owner, repo, number = pr
    try:
        result = subprocess.run(
            [
                "gh", "pr", "view", number,
                "--repo", f"{owner}/{repo}",
                "--json", "headRefName",
                "--jq", ".headRefName",
            ],
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError) as exc:
        detail = getattr(exc, "stderr", "") or str(exc)
        error(f"Could not resolve GitHub PR branch: {detail.strip()}")
        return

    branch = result.stdout.strip()
    if not branch:
        error("GitHub returned an empty PR branch name.")
        return

    repo_path = "/".join(quote(part, safe="") for part in (owner, repo))
    branch_path = quote(branch, safe="/")
    qute_command(
        f"open -t {SOURCEGRAPH}/r/github.com/{repo_path}/-/compare/...{branch_path}"
    )


if __name__ == "__main__":
    main()
