;;; notmuch-config.el --- notmuch specific configuration

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-c m") 'notmuch)

(setq
 ;; when archiving a thread I like to also mark it as read
 notmuch-archive-tags '("-inbox" "-unread")

 ;; on gmail sync we will get the file, we don't need to create the sent mail
 notmuch-fcc-dirs nil)

;; I like to jump to tag searches from notmuch-hello
(defun notmuch-search-by-tag (tag)
  "Display threads matching TAG in a notmuch-search buffer."
  (interactive
   (list (notmuch-select-tag-with-completion "Notmuch search tag: ")))
  (notmuch-search (concat "tag:" tag)))
(define-key notmuch-hello-mode-map "t" #'notmuch-search-by-tag)

;; mail sending via emacs. Relies on ~/.authinfo
(require 'smtpmail)
(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587

 ;; async
 smtpmail-queue-mail t
 smtpmail-queue-dir "~/.mail/queued-mail"

 ;; use personal as default
 user-mail-address   "keegan.csmith@gmail.com"
 smtpmail-smtp-user  "keegan.csmith@gmail.com"

 message-kill-buffer-on-exit t)

;; Use a message hook to decide which smtp details to use. I use gmail for
;; both my emails so I just need to adjust the smtp username.
(defun my-change-smtp-user ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (save-restriction
      (message-narrow-to-headers)
      (let* ((from (message-fetch-field "from"))
             (addr (cadr (mail-extract-address-components from))))
        (setq smtpmail-smtp-user addr)))))
(add-hook 'message-send-hook #'my-change-smtp-user)

;; Render Joe's name instead of his GitHub username.
(defun my-adjust-joe-name (args)
  "Replace Joe's unicode GitHub name in ARGS with normal width chars."
  (mapcar (apply-partially #'replace-regexp-in-string "ᴜɴᴋɴᴡᴏɴ" "Joe") args))

(advice-add 'notmuch-search-insert-authors :filter-args  #'my-adjust-joe-name)

;; Highlight my name in emails
(defun my-notmuch-wash-highlight-name (msg depth)
  "Highlight my name."
  (goto-char (point-min))
  (while (re-search-forward "@?\\(keegancsmith\\|keegan\\|kcsmith\\)" nil t)
    (add-text-properties (match-beginning 0) (point)
                         '(face match))))

(add-to-list 'notmuch-show-insert-text/plain-hook #'my-notmuch-wash-highlight-name t)

;; Mention if there are emails that need sending on hello page. Borrowed from
;; mu4e codebase.
(defun my-notmuch-hello-queued-mail ()
  "Modified version of mu4e~main-view-queue to just show queue size if non-empty"
  (let ((queue-size (my-mu4e~main-queue-size)))
    (unless (zerop queue-size)
      (insert (format "\n\n\t!!! [G] flush %s queued %s !!!\n\n"
                      (int-to-string queue-size)
                      (if (> queue-size 1) "mails" "mail"))))))

(defun my-mu4e~main-queue-size ()
  "Return, as an int, the number of emails in the queue."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents (expand-file-name smtpmail-queue-index-file
                                                smtpmail-queue-dir))
        (count-lines (point-min) (point-max)))
    (error 0)))

(defun my-notmuch-new-process-sentinel (proc msg)
  (message "Polling mail...done")
  (notmuch-hello-update))

(defun my-notmuch-send-poll-and-refresh-this-buffer ()
  "sends queued email and refreshes current notmuch buffer."
  (interactive)
  (smtpmail-send-queued-mail)
  (message "Polling mail...")
  (notmuch-start-notmuch "notmuch-new" nil #'my-notmuch-new-process-sentinel "new"))

(add-to-list 'notmuch-hello-sections #'my-notmuch-hello-queued-mail)
(define-key notmuch-hello-mode-map (kbd "G") #'my-notmuch-send-poll-and-refresh-this-buffer)

(defun my-notmuch-show-browse ()
  "Browse to the last URL in a thread. Useful for GitHub notifications."
  (interactive)
  (let* ((urls (notmuch-show--gather-urls))
         (url (car (last urls))))
    (if url (progn
              (browse-url url)
              (message "Visited %s" url))
      (ding)
      (message "No URLs found in message!")
      nil)))

(defun my-notmuch-search-browse ()
  "Browse to the last URL in a thread. Useful for GitHub notifications."
  (interactive)
  (when (save-window-excursion
          (notmuch-search-show-thread)
          (my-notmuch-show-browse))
    (notmuch-search-archive-thread)))

(define-key notmuch-show-mode-map "B" #'my-notmuch-show-browse)
(define-key notmuch-search-mode-map "B" #'my-notmuch-search-browse)

;;; notmuch-config.el ends here
