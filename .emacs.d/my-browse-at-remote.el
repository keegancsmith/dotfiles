;;; my-browse-at-remote.el --- Custom browse-at-remote with Sourcegraph support -*- lexical-binding: t -*-

(require 'browse-at-remote)

(defun my-browse-at-remote--format-region-url-as-github-advice (orig-fn repo-url location filename &optional linestart lineend)
  "Advice to produce Sourcegraph URLs for sourcegraph org repos.
With a prefix argument, uses the current branch/commit (location) instead of main."
  (if (string-match "github\\.com/\\(sourcegraph/[^/]+\\)" repo-url)
      (let* ((repo (match-string 1 repo-url))
             (branch (if current-prefix-arg location "main")))
        (cond
         ((and linestart lineend)
          (format "https://sourcegraph.sourcegraph.com/github.com/%s@%s/-/blob/%s?L%d-%d" repo branch filename linestart lineend))
         (linestart
          (format "https://sourcegraph.sourcegraph.com/github.com/%s@%s/-/blob/%s?L%d" repo branch filename linestart))
         (t
          (format "https://sourcegraph.sourcegraph.com/github.com/%s@%s/-/blob/%s" repo branch filename))))
    (funcall orig-fn repo-url location filename linestart lineend)))

(advice-add 'browse-at-remote--format-region-url-as-github :around #'my-browse-at-remote--format-region-url-as-github-advice)

(provide 'my-browse-at-remote)

;;; my-browse-at-remote.el ends here
