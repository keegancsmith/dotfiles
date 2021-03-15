config.load_autoconfig()

c.editor.command = ['/usr/local/bin/emacsclient', '+{line}:{column}', '{file}']

c.auto_save.session = True

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

# simple script to write org entry to my inbox.org
config.bind(';w', 'spawn --userscript ~/.qutebrowser/userscripts/org-capture.py')

# password_fill from upstream
config.bind(';p', 'spawn --userscript ~/.qutebrowser/userscripts/password_fill')

config.bind(';a', 'insert-text Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>')
