#!/usr/bin/env python3

'''Open up hackernews or reddit stories as background tabs. Opens up both the
story and comments.

Configure in qutebrowser with:

c.hints.selectors['story'] = [
    '.athing', # hn
    '.thing',  # reddit
]
config.bind(';s', 'hint --rapid story userscript hn.py')
'''

__author__ = 'Keegan Carruthers-Smith'

import os

from collections import defaultdict
from html.parser import HTMLParser


def extract_links(html):
    class Parser(HTMLParser):
        def __init__(self):
            super().__init__()
            self.path = []
            self.story = None
            self.comments = None

        def handle_starttag(self, tag, attrs):
            attrs = defaultdict(str, attrs)

            # HN container
            if 'athing' in attrs['class'].split():
                self.comments = 'https://news.ycombinator.com/item?id=' + attrs['id']

            # HN story
            if (
                tag == 'a'
                and len(self.path) >= 1
                and self.path[-1]['class'] == 'titleline'
            ):
                self.story = attrs['href']
                if self.story.startswith('item?id'):
                    self.story = 'https://news.ycombinator.com/' + self.story

            # Reddit makes it easy with data attributes
            if 'thing' in attrs['class'].split():
                if attrs['data-url'] == attrs['data-permalink']:
                    self.story = 'https://old.reddit.com' + attrs['data-url']
                else:
                    self.story = attrs['data-url']
                    self.comments = 'https://old.reddit.com' + attrs['data-permalink']

            attrs['tag'] = tag
            self.path.append(attrs)

        def handle_endtag(self, tag):
            self.path.pop()

    p = Parser()
    p.feed(html)

    if not p.story:
        return ()

    if not p.comments or p.comments == p.story:
        return (p.story,)

    return (p.story, p.comments)


def test_extract_links():
    cases = (
        (
            '''<tr class='athing' id='34847941'>
      <td align="right" valign="top" class="title"><span class="rank">5.</span></td>      <td valign="top" class="votelinks"><center><a id='up_34847941' class='clicky' href='vote?id=34847941&amp;how=up&amp;auth=d60a1480f474050745304452bfccf81cecb7650a&amp;goto=news'><div class='votearrow' title='upvote'></div></a></center></td><td class="title"><span class="titleline"><a href="https://forum.nim-lang.org/t/9906">Returning to Nim from Python and Rust</a><span class="sitebit comhead"> (<a href="from?site=nim-lang.org"><span class="sitestr">nim-lang.org</span></a>)</span></span></td></tr><tr><td colspan="2"></td><td class="subtext"><span class="subline">
          <span class="score" id="score_34847941">45 points</span> by <a href="user?id=planetis" class="hnuser">planetis</a> <span class="age" title="2023-02-18T16:09:26"><a href="item?id=34847941">1 hour ago</a></span> <span id="unv_34847941"></span> | <a href="flag?id=34847941&amp;auth=d60a1480f474050745304452bfccf81cecb7650a&amp;goto=news">flag</a> | <a href="hide?id=34847941&amp;auth=d60a1480f474050745304452bfccf81cecb7650a&amp;goto=news" class="clicky">hide</a> | <a href="item?id=34847941">25&nbsp;comments</a>        </span>
              </td></tr>''',
            (
                'https://forum.nim-lang.org/t/9906',
                'https://news.ycombinator.com/item?id=34847941',
            ),
        ),
        (
            '''<tr class="athing submission" id="42174829">
      <td align="right" valign="top" class="title"><span class="rank">2.</span></td>      <td valign="top" class="votelinks"><center><a id="up_42174829" href="vote?id=42174829&amp;how=up&amp;goto=news"><div class="votearrow" title="upvote"></div></a></center></td><td class="title"><span class="titleline"><a href="https://github.com/circlemind-ai/fast-graphrag">Show HN: FastGraphRAG – Better RAG using good old PageRank</a><span class="sitebit comhead"> (<a href="from?site=github.com/circlemind-ai"><span class="sitestr">github.com/circlemind-ai</span></a>)</span></span></td></tr>''',
            (
                'https://github.com/circlemind-ai/fast-graphrag',
                'https://news.ycombinator.com/item?id=42174829',
            ),
        ),
        (
            '''<tr class="athing" id="34612353">
      <td align="right" valign="top" class="title"><span class="rank">4.</span></td>      <td valign="top" class="votelinks"><center><a id="up_34612353" class="clicky" href="vote?id=34612353&amp;how=up&amp;auth=8d07089fc1d43edd92581589c8519e1651d28e56&amp;goto=front%3Fday%3D2023-02-01"><div class="votearrow" title="upvote"></div></a></center></td><td class="title"><span class="titleline"><a href="item?id=34612353">Ask HN: Who is hiring? (February 2023)</a></span></td></tr>''',
            ('https://news.ycombinator.com/item?id=34612353',),
        ),
        (
            '''<div class=" thing id-t3_114rpif odd  link " id="thing_t3_114rpif" onclick="click_thing(this)" data-fullname="t3_114rpif" data-type="link" data-gildings="0" data-whitelist-status="all_ads" data-is-gallery="false" data-author="Alexander_Selkirk" data-author-fullname="t2_rvxk0" data-subreddit="programming" data-subreddit-prefixed="r/programming" data-subreddit-fullname="t5_2fwo" data-subreddit-type="public" data-timestamp="1676653107000" data-url="http://sevangelatos.com/john-carmack-on/" data-permalink="/r/programming/comments/114rpif/john_carmack_on_functional_programming_in_c/" data-domain="sevangelatos.com" data-rank="3" data-comments-count="365" data-score="2319" data-promoted="false" data-nsfw="false" data-spoiler="false" data-oc="false" data-num-crossposts="2" data-context="listing"><p class="parent"></p><span class="rank">3</span><div class="midcol unvoted"><div class="arrow up login-required access-required" data-event-action="upvote" role="button" aria-label="upvote" tabindex="0"></div><div class="score dislikes" title="2318">2318</div><div class="score unvoted" title="2319">2319</div><div class="score likes" title="2320">2320</div><div class="arrow down login-required access-required" data-event-action="downvote" role="button" aria-label="downvote" tabindex="0"></div></div><div class="entry unvoted"><div class="top-matter"><p class="title"><a class="title may-blank loggedin " data-event-action="title" href="http://sevangelatos.com/john-carmack-on/" tabindex="1">John Carmack on Functional Programming in C++</a> <span class="domain">(<a href="/domain/sevangelatos.com/">sevangelatos.com</a>)</span></p><p class="tagline ">submitted <time title="Fri Feb 17 16:58:27 2023 UTC" datetime="2023-02-17T16:58:27+00:00" class="">1 day ago</time> by <a href="https://old.reddit.com/user/Alexander_Selkirk" class="author may-blank id-t2_rvxk0">Alexander_Selkirk</a><span class="userattrs"></span><span class="awardings-bar" data-subredditpath="/r/programming/"></span></p><ul class="flat-list buttons"><li class="first"><a href="https://old.reddit.com/r/programming/comments/114rpif/john_carmack_on_functional_programming_in_c/" data-event-action="comments" class="bylink comments may-blank" rel="nofollow">365 comments</a></li><li class="share"><a class="post-sharing-button" href="javascript: void 0;">share</a></li><li class="link-save-button save-button login-required"><a href="#">save</a></li><li><form action="/post/hide" method="post" class="state-button hide-button"><input type="hidden" name="executed" value="hidden"><span><a href="javascript:void(0)" class=" " data-event-action="hide" onclick="change_state(this, 'hide', hide_thing);">hide</a></span></form></li><li class="give-gold-button"><a href="/gold?goldtype=gift&amp;months=1&amp;thing=t3_114rpif" title="give an award in appreciation of this post." class="give-gold login-required access-required gold-give-gold" data-event-action="gild" data-community-awards-enabled="True" rel="nofollow">give award</a></li><li class="report-button login-required"><a href="javascript:void(0)" class="reportbtn access-required" data-event-action="report">report</a></li><li class="crosspost-button"><a class="post-crosspost-button" href="javascript: void 0;" data-crosspost-fullname="t3_114rpif">crosspost</a></li></ul><div class="reportform report-t3_114rpif"></div></div></div><div class="child"></div><div class="clearleft"></div></div>''',
            (
                'http://sevangelatos.com/john-carmack-on/',
                'https://old.reddit.com/r/programming/comments/114rpif/john_carmack_on_functional_programming_in_c/',
            ),
        ),
        (
            '''<div class=" thing id-t3_112t0uo odd  stickied link self" id="thing_t3_112t0uo" onclick="click_thing(this)" data-fullname="t3_112t0uo" data-type="link" data-gildings="0" data-whitelist-status="all_ads" data-is-gallery="false" data-author="AutoModerator" data-author-fullname="t2_6l4z3" data-subreddit="emacs" data-subreddit-prefixed="r/emacs" data-subreddit-fullname="t5_2qhwu" data-subreddit-type="public" data-timestamp="1676448909000" data-url="/r/emacs/comments/112t0uo/weekly_tips_tricks_c_thread/" data-permalink="/r/emacs/comments/112t0uo/weekly_tips_tricks_c_thread/" data-domain="self.emacs" data-rank="" data-comments-count="12" data-score="14" data-promoted="false" data-nsfw="false" data-spoiler="false" data-oc="false" data-num-crossposts="0" data-context="listing"><p class="parent"></p><span class="rank"></span><div class="midcol unvoted"><div class="arrow up login-required access-required" data-event-action="upvote" role="button" aria-label="upvote" tabindex="0"></div><div class="score dislikes" title="13">13</div><div class="score unvoted" title="14">14</div><div class="score likes" title="15">15</div><div class="arrow down login-required access-required" data-event-action="downvote" role="button" aria-label="downvote" tabindex="0"></div></div><div class="entry unvoted"><div class="top-matter"><p class="title"><a class="title may-blank loggedin " data-event-action="title" href="/r/emacs/comments/112t0uo/weekly_tips_tricks_c_thread/" tabindex="1">Weekly Tips, Tricks, &amp;c. Thread</a> <span class="domain">(<a href="/r/emacs/">self.emacs</a>)</span></p><div class="expando-button collapsed hide-when-pinned selftext"></div><p class="tagline ">submitted <time title="Wed Feb 15 08:15:09 2023 UTC" datetime="2023-02-15T08:15:09+00:00" class="">3 days ago</time> by <a href="https://old.reddit.com/user/AutoModerator" class="author may-blank id-t2_6l4z3">AutoModerator</a><span class="userattrs"></span><span class="awardings-bar" data-subredditpath="/r/emacs/"></span> - <span class="stickied-tagline" title="selected by this subreddit's moderators">announcement</span></p><ul class="flat-list buttons"><li class="first"><a href="https://old.reddit.com/r/emacs/comments/112t0uo/weekly_tips_tricks_c_thread/" data-event-action="comments" class="bylink comments may-blank" rel="nofollow">12 comments</a></li><li class="share"><a class="post-sharing-button" href="javascript: void 0;">share</a></li><li class="link-save-button save-button login-required"><a href="#">save</a></li><li><form action="/post/hide" method="post" class="state-button hide-button"><input type="hidden" name="executed" value="hidden"><span><a href="javascript:void(0)" class=" " data-event-action="hide" onclick="change_state(this, 'hide', hide_thing);">hide</a></span></form></li><li class="give-gold-button"><a href="/gold?goldtype=gift&amp;months=1&amp;thing=t3_112t0uo" title="give an award in appreciation of this post." class="give-gold login-required access-required gold-give-gold" data-event-action="gild" data-community-awards-enabled="True" rel="nofollow">give award</a></li><li class="report-button login-required"><a href="javascript:void(0)" class="reportbtn access-required" data-event-action="report">report</a></li><li class="crosspost-button"><a class="post-crosspost-button" href="javascript: void 0;" data-crosspost-fullname="t3_112t0uo">crosspost</a></li></ul><div class="reportform report-t3_112t0uo"></div></div><div class="expando expando-uninitialized" style="display: none" data-pin-condition="function() {return this.style.display != 'none';}"><span class="error">loading...</span></div></div><div class="child"></div><div class="clearleft"></div></div>''',
            (
                'https://old.reddit.com/r/emacs/comments/112t0uo/weekly_tips_tricks_c_thread/',
            ),
        ),
    )
    for html, want in cases:
        got = extract_links(html)
        if want != got:
            print(want)
            print(got)
            assert False


def main():
    links = extract_links(os.getenv('QUTE_SELECTED_HTML'))

    if not links:
        return

    with open(os.environ['QUTE_FIFO'], 'a') as fifo:
        for link in links:
            fifo.write(f'open -b {link}\n')
        fifo.flush()


if __name__ == '__main__':
    main()
