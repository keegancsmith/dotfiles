from mercurial import commands

def post_push(ui, repo, pats, opts, *args, **kwargs):
    dest = pats and pats[0]
    dest = ui.expandpath(dest or 'default-push', dest or 'default')
    if 'bitbucket.org' in dest:
        github = ui.config('paths', 'github')
        if github:
            return commands.push(ui, repo, github, **opts)
        ui.warn('no github mirror!?\n')
