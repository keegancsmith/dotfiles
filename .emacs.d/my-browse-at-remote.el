;;; my-browse-at-remote.el --- Custom browse-at-remote with Sourcegraph support -*- lexical-binding: t -*-

(require 'browse-at-remote)

(defun my-github-to-sourcegraph-url (github-url)
  "Convert GITHUB-URL to Sourcegraph URL for sourcegraph repos."
  (when (string-match "^\\(https?://\\)github\\.com/\\(sourcegraph/[^/]+\\)/blob/[^/]+/\\(.+\\)$" github-url)
    (let* ((repo (match-string 2 github-url))
           (path (match-string 3 github-url))
           (path-and-lines (split-string path "#"))
           (filepath (car path-and-lines))
           (lines (cadr path-and-lines)))
      (concat "https://sourcegraph.sourcegraph.com/github.com/" repo "/-/blob/" filepath
              (when lines
                (concat "?" (replace-regexp-in-string "L\\([0-9]+\\)-L\\([0-9]+\\)" "L\\1-\\2" lines)))))))

(defun my-browse-at-remote-get-url-advice (url)
  "Advice to convert GitHub URLs to Sourcegraph for sourcegraph repos."
  (if (string-match-p "sourcegraph" url)
      (or (my-github-to-sourcegraph-url url) url)
    url))

(advice-add 'browse-at-remote-get-url :filter-return #'my-browse-at-remote-get-url-advice)

(provide 'my-browse-at-remote)

;;; my-browse-at-remote.el ends here
