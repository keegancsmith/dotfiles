#!/usr/bin/env python3

'''Simple org-capture userscript setup for my needs.'''

__author__ = 'Keegan Carruthers-Smith'

import datetime
import os
import os.path
import re

def remove_prefix(s, prefix):
    if s.startswith(prefix):
        return s[len(prefix):]
    return s

def org_headline_github(url, title):
    p = url.split('/')
    if len(p) < 7 or p[2] != 'github.com' or p[5] not in ('pull', 'issues'):
        return None

    repo = p[3] + '/' + p[4]
    pid  = p[6]

    repo = remove_prefix(repo, 'sourcegraph/')
    if repo == 'sourcegraph':
        repo = ''

    p = title.split(' · ')
    title = p[0].strip()

    if m := re.match(r'^(\(\d+\) )?(.*?)( by \w+)?$', title):
        title = m.group(2)

    return f'{title} [[{url}][{repo}#{pid}]]'

def org_headline_gdocs(url, title):
    m = re.match(r'RFC (\d+)[^:]*: (.+) - Google Docs', title)
    if m is None:
        return None

    rfc = m.group(1)
    title = m.group(2).strip()

    if (idx := url.index('/edit')) >= 0:
        url = url[:idx]

    return f'RFC {rfc}: {title} [[{url}][RFC {rfc}]]'

def org_headline(url, title):
    for f in (org_headline_github, org_headline_gdocs):
        org = f(url, title)
        if org is not None:
            return org
    return org

def test_org_headline():
    cases = ((
        'https://github.com/sourcegraph/zoekt/pull/10',
        'index: Use roaring bitmaps for content posting lists by keegancsmith · Pull Request #10 · sourcegraph/zoekt',
        'index: Use roaring bitmaps for content posting lists [[https://github.com/sourcegraph/zoekt/pull/10][zoekt#10]]',
    ), (
        'https://github.com/sourcegraph/sourcegraph/issues/6031',
        'Core Services: 3.10 tracking issue · Issue #6031 · sourcegraph/sourcegraph',
        'Core Services: 3.10 tracking issue [[https://github.com/sourcegraph/sourcegraph/issues/6031][#6031]]',
    ), (
        'https://docs.google.com/document/d/18w8T_KzYxQye8wg1g01QpMOX4_ERTtbOxMBRYaOEkmk/edit#heading=h.rf2d3v2oo71l',
        'RFC 30: Zoekt Horizontal Scaling - Google Docs',
        'RFC 30: Zoekt Horizontal Scaling [[https://docs.google.com/document/d/18w8T_KzYxQye8wg1g01QpMOX4_ERTtbOxMBRYaOEkmk][RFC 30]]',
    ))
    for url, title, want in cases:
        got = org_headline(url, title)
        assert want == got
    
def org_entry(url, title):
    time = datetime.datetime.now().strftime('[%Y-%m-%d %a %H:%M]')
    if headline := org_headline(url, title):
        return f'* TODO {headline}\n{time}\n'
    return f'* TODO {title}\n{time}\n{url}\n'

def main():
    url = os.getenv('QUTE_URL')
    title = os.getenv('QUTE_TITLE')
    inbox = os.path.expanduser('~/org-files/inbox.org')

    entry = org_entry(url, title)

    with open(inbox, 'r') as f:
        has_trailing_newline = f.read()[-1] == '\n'
    if not has_trailing_newline:
        entry = '\n' + entry
    
    with open(inbox, 'a') as f:
        f.write(entry)

    with open(os.environ['QUTE_FIFO'], 'a') as fifo:
        message = f'captured to inbox.org'
        fifo.write(f'message-info {message!r}\n')
        fifo.flush()

    # Always append to the tmp file, just incase I worry that org-files
    # overwrote what I put into inbox.org
    with open('/tmp/inbox-log.org', 'a') as f:
        f.write(entry)


if __name__ == '__main__':
    main()
