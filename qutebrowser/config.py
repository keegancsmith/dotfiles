config.load_autoconfig()

c.window.hide_decoration = True

c.editor.command = ['/usr/local/bin/emacsclient', '+{line}:{column}', '{file}']

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
    'a[href^="item"]', # hn comment link
]
config.bind(';s', 'hint --rapid story tab-bg')
