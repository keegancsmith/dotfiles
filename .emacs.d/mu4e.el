(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(require 'mu4e)

;;store org-mode links to messages
(require 'org-mu4e)

;; when mail is sent, automatically convert org body to HTML
(setq org-mu4e-convert-to-html t)

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;; I want my format=flowed thank you very much
;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
;; each paragraph is a single long line; at sending, emacs will add the
;; special line continuation characters.
(setq mu4e-compose-format-flowed t)

;; every new email composition gets its own frame! (window)
(setq mu4e-compose-in-new-frame t)

;; give me ISO(ish) format date-time stamps in the header list
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; rename files when moving for mbsync
(setq mu4e-change-filenames-when-moving t)

(setq mu4e-attachment-dir "~/Downloads")

;; path to our Maildir directory
(setq mu4e-maildir "~/Maildir")

;; the next are relative to `mu4e-maildir'
;; instead of strings, they can be functions too, see
;; their docstring or the chapter 'Dynamic folders'
;(setq mu4e-sent-folder   "/Sent"
;      mu4e-drafts-folder "/Drafts"
;      mu4e-trash-folder  "/Trash")

;; the maildirs you use frequently; access them with 'j' ('jump')
(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX"                . ?i)
        ("/sourcegraph/INBOX"          . ?w)))

;; the list of all of my e-mail addresses
(setq mu4e-user-mail-address-list '("keegan.csmith@gmail.com"
                                    "keegan@sourcegraph.com"))

;; the headers to show in the headers list -- a pair of a field
;; and its width, with `nil' meaning 'unlimited'
;; (better only use that for the last field.
;; These are the defaults:
(setq mu4e-headers-fields
    '( (:date          .  25)    ;; alternatively, use :human-date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  nil))) ;; alternatively, use :thread-subject

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
;; when I press U in the main view, or C-c C-u elsewhere,
;; this command is called, followed by the mu indexer
(setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")

;; not using smtp-async yet
;; some of these variables will get overridden by the contexts
;(setq
; send-mail-function 'smtpmail-send-it
; message-send-mail-function 'smtpmail-send-it
; smtpmail-smtp-server "smtp.fastmail.com"
; smtpmail-smtp-service 465
; smtpmail-stream-type 'ssl
; )

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; here come the contexts
;; I have about 5 of these, chopped down to 2 for demonstration purposes
;; each context can set any number of variables (see :vars)
;; for example below here I'm using two different SMTP servers depending on identity
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "c keegan.csmith@gmail.com"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/gmail/" (mu4e-message-field msg :maildir))))
           :vars '(
                   (user-mail-address  . "keegan.csmith@gmail.com")
                   (smtpmail-smtp-user . "keegan.csmith@gmail.com")
                   (mu4e-sent-folder   . "/gmail/[Gmail]/Sent Mail")
                   (mu4e-trash-folder  . "/gmail/[Gmail]/Bin")
                   (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
                   ))

         ,(make-mu4e-context
           :name "s keegan@sourcegraph.com"
           :match-func (lambda (msg)
                         (when msg
                           (string-prefix-p "/sourcegraph/" (mu4e-message-field msg :maildir))))
           :vars '(
                   (user-mail-address  . "keegan@sourcegraph.com" )
                   (smtpmail-smtp-user . "keegan@sourcegraph.com")
                   (mu4e-sent-folder   . "/sourcegraph/[Gmail]/Sent Mail")
                   (mu4e-trash-folder  . "/sourcegraph/[Gmail]/Trash")
                   (mu4e-refile-folder . "/sourcegraph/[Gmail]/All Mail")
                   ))))

;; start with the first (default) context;
(setq mu4e-context-policy 'pick-first)

;; compose with the current context if no context matches;
(setq mu4e-compose-context-policy nil)

;; these are the standard mu4e search bookmarks
;; I've only added the fourth one to pull up flagged emails in my inbox
;; I sometimes use this to shortlist emails I need to get around to ASAP
(setq mu4e-bookmarks
      `(
        ,(make-mu4e-bookmark
          :name  "Inbox"
          :query "maildir:/inbox/ AND NOT flag:trashed"
          :key   ?i)
        ,(make-mu4e-bookmark
          :name  "All"
          :query "maildir:\"/All Mail/\" AND NOT flag:trashed"
          :key   ?a)
        ,(make-mu4e-bookmark
          :name  "Unread messages"
          :query "flag:unread AND NOT flag:trashed"
          :key ?u)
        ,(make-mu4e-bookmark
          :name "Today's messages"
          :query "date:today..now AND NOT flag:trashed"
          :key ?t)
        ,(make-mu4e-bookmark
          :name "Last 7 days"
          :query "date:7d..now AND NOT flag:trashed"
          :key ?w)
        ,(make-mu4e-bookmark
          :name "Flagged"
          :query "flag:flagged"
          :key ?f)))

;; make html to txt conversion visible on dark theme
;(setq shr-color-visible-luminance-min 80)

(add-to-list 'mu4e-view-actions
             '("browser" . mu4e-action-view-in-browser) t)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "keegan.csmith@gmail.com" nil)
                                  ("smtp.gmail.com" 587 "keegan@sourcegraph.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
