;;; my-linear-issues.el --- assigned linear issues to org-mode

;;; Commentary:

;;; Code:

(require 'auth-source)
(require 'json)
(require 'org)
(require 'url)

(defun my-linear-issues--graphql-request (query)
  ""
  (let* ((auth (my-linear-issues--access-token))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Accept" . "application/json")
                                      ("Authorization" . ,auth)))
         (url-request-data (json-encode `(:query ,query)))
         (url "https://api.linear.app/graphql")
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (let ((json-object-type 'plist)
              (json-key-type 'keyword)
              (json-array-type 'vector))
          (json-read-from-string
           (buffer-substring (point) (point-max))))))))

(defun my-linear-issues--access-token ()
  ""
  (or (auth-source-pick-first-password
       :host "api.linear.app"
       :require '(:secret :host))
      (error "Could not find access token for linear")))

(defun my-linear-issues ()
  ""
  (interactive)

  (let (data issues states)

    (setq data (my-linear-issues--graphql-request "{
  viewer {
    assignedIssues(filter: { completedAt: { null: true } }) {
      nodes {
        title
        url
        identifier
        state {
          name
          position
        }
      }
    }
  }
}"))

    ;; Get at the issue nodes
    (setq issues (seq-reduce #'plist-get '(:data :viewer :assignedIssues :nodes) data))

    ;; We want to create a heading per workflow state (eg "Todo" "In Review" ...)
    (setq states (seq-map (lambda (issue) (plist-get issue :state)) issues))
    (delete-dups states)
    (setq states (seq-sort-by (lambda (state) (plist-get state :position)) #'< states))

    (pop-to-buffer (get-buffer-create "linear-issues.org"))
    (erase-buffer)
    (unless (eq major-mode 'org-mode)
      (org-mode))

    (seq-doseq (state states)
      (insert "* " (encode-coding-string (plist-get state :name) 'utf-8) "\n")
      (seq-doseq (issue issues)
        (when (equal state (plist-get issue :state))
          (insert "** TODO ")
          (org-insert-link nil (plist-get issue :url) (plist-get issue :identifier))
          (insert " " (plist-get issue :title) "\n"))))))

;;; my-linear-issues.el ends here
