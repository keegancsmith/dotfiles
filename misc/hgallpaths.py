# hgallpaths.py - pull and push to all paths
#
# Copyright 2012 Keegan Carruthers-Smith <keegan.csmith@gmail.com>
#
# Released under the terms of the BSD License. See LICENSE.txt for details.

'''push and pull to all paths'''

from mercurial.i18n import _
from mercurial import commands, util

cmdtable = {}


def do_command(command, path_kw, ui, *args, **opts):
    cmd = getattr(commands, command)
    exclude = set(ui.configlist('hgallpaths', 'exclude', []) +
                  ui.configlist('hgallpaths', 'exclude_%s' % command, []))
    paths = ui.configitems('paths')
    paths = [(name, path) for name, path in paths if name not in exclude]
    path_names = ', '.join(name for name, path in paths)

    if paths:
        ui.status(_('%s to %s\n') % (command, path_names))
    else:
        raise util.Abort(_('No paths to %s') % command)

    for name, path in paths:
        if name not in exclude:
            opts[path_kw] = path
            cmd(ui, *args, **opts)


def create_command(command, path_kw):
    def cmd(*args, **opts):
        return do_command(command, path_kw, *args, **opts)
    cmd.__doc__ = 'See help for %s' % command

    global cmdtable
    cmdtable[command + 'all'] = (cmd, [])
    return cmd

pullall = create_command('pull', 'source')
pushall = create_command('push', 'dest')
