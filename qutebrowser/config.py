config.load_autoconfig()

# Add /usr/local/bin to PATH
import os
if '/usr/local/bin:' not in os.environ['PATH']:
    os.environ['PATH'] = '/usr/local/bin:' + os.environ['PATH']

c.editor.command = ['emacsclient', '+{line}:{column}', '{file}']

c.auto_save.session = True

c.content.autoplay = False

# dir or hasattr doesn't work on the config classes, so this is how we check
# if a config option exists.
from qutebrowser.config import configdata

def enableClipboard(pattern):
    with config.pattern(pattern) as p:
        if 'content.javascript.clipboard' in configdata.DATA: # PyQT6
            p.content.javascript.clipboard = 'access'
        else: # Older API
            p.content.javascript.can_access_clipboard = True

enableClipboard('*://github.com')
enableClipboard('*://sourcegraph.grafana.net')
enableClipboard('*://pkg.go.dev')

# I pretty much never say yes here. Also bypasses sourcegraph dev env wanting
# to speak to wss even on http.
c.content.tls.certificate_errors = 'ask-block-thirdparty'

c.url.searchengines = {
    'DEFAULT': 'https://www.google.com/search?q={}',
    's': 'https://sourcegraph.sourcegraph.com/search?q=context:global+{}&patternType=regexp',
    'ss': 'https://sourcegraph.com/search?q={}',
}

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

# simple script to write org entry to my inbox.org
config.bind(',w', 'spawn --userscript ' + userscript('org-capture.py'))

# password_fill from upstream
config.bind(',p', 'spawn --userscript ' + userscript('password_fill'))

# send URL to to either macbook or linux desktop
import platform
if platform.node() == 'habitat':
    config.bind(',m', 'spawn ssh real open {url}')
else:
    config.bind(',m', 'spawn ssh habitat DISPLAY=:0 xdg-open {url}')

# get URL from my work macbook
config.bind(',M', 'spawn --userscript ' + userscript('get_url.sh'))

# Open in chrome
if platform.system() == 'Darwin':
    config.bind(',c', 'spawn /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome {url}')
else:
    config.bind(',c', 'spawn google-chrome-stable {url}')

# JH says this is what the cool kids do
config.bind('tt', 'set-cmd-text -s :tab-select')

config.bind(',a', 'insert-text Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>')
config.bind(',A', 'insert-text Co-authored-by: @stefanhengl')
