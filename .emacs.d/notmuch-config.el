;;; notmuch-config.el --- notmuch specific configuration

;;; Commentary:

;;; Code:

(global-set-key (kbd "C-c m") 'notmuch)

;; org notmuch links
(require 'ol-notmuch)

(setq
 ;; when archiving a thread I like to also mark it as read
 notmuch-archive-tags '("-inbox" "-unread")

 ;; prefer showing newest first
 notmuch-search-oldest-first nil

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

;;; notmuch-config.el ends here
