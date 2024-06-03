#!/usr/bin/env python3

'''userscript to open code based on URL setup for my needs.'''

__author__ = 'Keegan Carruthers-Smith'

import os
import sys
import subprocess

from pathlib import Path
from urllib.parse import urlparse


def extract_filepath(url):
    u = urlparse(url)
    if 'sourcegraph.com' in u.hostname:
        repo, path = u.path.split('/-/blob/', 1)
        repo = repo.lstrip('/')
        if '@' in repo:
            repo = repo[:repo.index('@')]
        return repo + '/' + path
    if 'github.com' in u.hostname:
        parts = u.path.split('/')
        assert parts[0] == ''
        assert parts[3] == 'blob'
        parts.pop(4)  # branch/rev
        parts.pop(3)  # "blob"
        parts.pop(0)  # ""
        parts.insert(0, 'github.com')
        return '/'.join(parts)
    assert False, 'unsupported URL: ' + url


def test_extract_filepath():
    want = 'github.com/org/name/dir/file.txt'
    cases = (
        'https://sourcegraph.com/github.com/org/name/-/blob/dir/file.txt',
        'https://sourcegraph.sourcegraph.com/github.com/org/name/-/blob/dir/file.txt',
        'https://sourcegraph.sourcegraph.com/github.com/org/name@deedbeef/-/blob/dir/file.txt',
        'https://github.com/org/name/blob/main/dir/file.txt',
        'https://github.com/org/name/blob/deadbeef/dir/file.txt',
    )
    for url in cases:
        got = extract_filepath(url)
        assert want == got, f'{url}\ngot  {got}\nwant {want}'


def main():
    url = os.getenv('QUTE_URL')
    path = Path.home() / 'src' / extract_filepath(url)
    print(f'opening {path}')
    subprocess.call(sys.argv[1:] + [path])


if __name__ == '__main__':
    main()

# test_extract_filepath()
