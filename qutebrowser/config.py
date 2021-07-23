config.load_autoconfig()

# Add /usr/local/bin to PATH
import os
if '/usr/local/bin:' not in os.environ['PATH']:
    os.environ['PATH'] = '/usr/local/bin:' + os.environ['PATH']

c.editor.command = ['emacsclient', '+{line}:{column}', '{file}']

c.auto_save.session = True

c.url.searchengines = {
    'DEFAULT': 'https://search.brave.com/search?q={}',
    'g': 'https://www.google.com/search?q={}',
    's': 'https://sourcegraph.com/search?q=context:global+{}&patternType=regexp',
    'ss': 'https://sourcegraph.com/search?q=context:%40keegan+{}&patternType=regexp',
}

# stop closing tabs by mistake
config.unbind("d")
config.bind(",d", "tab-close")

# mpv
config.bind(';m', 'spawn mpv {url}')
config.bind(';M', 'hint links spawn mpv {hint-url}')

# comment sections of reddit/hackernews
c.hints.selectors['comments'] = [
    '[class*="expand"]',
    '[class="togg"]',
    '[class="comment-meta"]',
]
config.bind(';c', 'hint comments')

# links on reddit/hackernews
c.hints.selectors['story'] = [
    'a[class~="title"]',
    'a[class~="storylink"]',
    'a[class~="comments"]',
    '.subtext > a[href^="item"]', # hn comment link
]
config.bind(';s', 'hint --rapid story tab-bg')

import os.path
userscript = lambda p : os.path.join(os.path.dirname(__file__), 'userscripts', p)

# simple script to write org entry to my inbox.org
config.bind(';w', 'spawn --userscript ' + userscript('org-capture.py'))

# password_fill from upstream
config.bind(';p', 'spawn --userscript ' + userscript('password_fill'))

config.bind(';a', 'insert-text Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>')
