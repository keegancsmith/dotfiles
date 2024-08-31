config.load_autoconfig()

# Add nix controlled tools to PATH
import os
if '/var/run/current-system/sw/bin:' not in os.environ['PATH']:
    os.environ['PATH'] = '/var/run/current-system/sw/bin:' + os.environ['PATH']

c.editor.command = ['emacsclient', '+{line}:{column}', '{file}']

c.auto_save.session = True

c.content.autoplay = False
c.content.blocking.method = 'both'

c.window.hide_decoration = True

def enableClipboard(pattern):
    with config.pattern(pattern) as p:
        p.content.javascript.clipboard = 'access'

enableClipboard('*://github.com')
enableClipboard('*://sourcegraph.grafana.net')
enableClipboard('*://pkg.go.dev')
enableClipboard('*://login.tailscale.com')

# I pretty much never say yes here. Also bypasses sourcegraph dev env wanting
# to speak to wss even on http.
c.content.tls.certificate_errors = 'ask-block-thirdparty'

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    's': 'https://sourcegraph.com/search?patternType=keyword&q={}',
    's2': 'https://sourcegraph.sourcegraph.com/search?patternType=keyword&q={}',
}

# Common repos I search
import re
import urllib.parse
sourcegraph = {
    'ss': 'sourcegraph/sourcegraph',
    'sc': 'sourcegraph/cody',
    'sg': 'golang/go',
    'sn': 'NixOS/nixpkgs',
    'sv': 'microsoft/vscode',
}
for k, repo in sourcegraph.items():
    q = 'repo:^' + re.escape('github.com/' + repo) + '$'
    host = 'sourcegraph.sourcegraph.com' if 'sourcegraph' in repo else 'sourcegraph.com'
    url_prefix = f'https://{host}/search?patternType=keyword&q={urllib.parse.quote_plus(q)}'
    c.url.searchengines[k] = url_prefix + '+{}'

# yank org-mode link
config.bind('yo', 'yank inline [[{url}][{title}]]')

# stop closing tabs by mistake
config.unbind("d")
config.bind(",d", "tab-close")

# mpv
config.bind(';m', 'spawn mpv {url}')
config.bind(';M', 'hint links spawn mpv {hint-url}')

# comment sections of reddit/hackernews
c.hints.selectors['comments'] = [
    '[class*="expand"]',
    '.togg', # hn
    '[class="comment-meta"]',
]
config.bind(';c', 'hint comments')

import os.path
userscript = lambda p : os.path.join(os.path.dirname(__file__), 'userscripts', p)

# links on reddit/hackernews
c.hints.selectors['story'] = [
    '.athing', # hn
    '.thing',  # reddit
]
config.bind(';s', 'hint --rapid story userscript ' + userscript('hn.py'))

# Open in emacs or vscode
config.bind(',e', 'spawn --userscript ' + userscript('open-code.py') + ' emacsclient --no-wait')
config.bind(',v', 'spawn --userscript ' + userscript('open-code.py') + ' code')

# simple script to write org entry to my inbox.org
config.bind(',w', 'spawn --userscript ' + userscript('org-capture.py'))

# password_fill from upstream
config.bind(',p', 'spawn --userscript ' + userscript('password_fill'))

# send URL to to either macbook or linux desktop
import platform
if platform.node() == 'habitat':
    config.bind(',m', 'spawn ssh fa.local open -b com.google.chrome {url}')
else:
    config.bind(',m', 'spawn ssh habitat DISPLAY=:0 xdg-open {url}')

# get URL from my work macbook
config.bind(',M', 'spawn --userscript ' + userscript('get_url.sh'))

# Open in chrome
if platform.system() == 'Darwin':
    config.bind(',c', 'spawn open -b com.google.chrome {url}')
    config.bind(',C', 'spawn open -a Safari {url}')
else:
    config.bind(',c', 'spawn google-chrome-stable {url}')

# JH says this is what the cool kids do
config.bind('tt', 'set-cmd-text -s :tab-select')

config.bind(',a', 'insert-text Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>')
config.bind(',A', 'insert-text Co-authored-by: @stefanhengl')
