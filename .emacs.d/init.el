;;; init.el --- Initialization file for Emacs

;;; Commentary:

;;; Code:

;; straight.el bootstrap code
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(when (memq window-system '(mac ns x))
  (set-frame-font "-*-Input Mono Narrow-normal-normal-extracondensed-*-12-*-*-*-m-0-iso10646-1")
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8")
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GOROOT")))

(server-start)

;; General config
(setq user-full-name "Keegan Carruthers-Smith")
(setq user-mail-address "keegan.csmith@gmail.com")
;; Highlight matching paren
(show-paren-mode 1)
;; Get rid of stupid GUI stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Yes-or-No queries become Y-or-N
(fset 'yes-or-no-p 'y-or-n-p)
;; Better buffer names when names conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix nil)
;; Misc settings
(setq
 vc-handled-backends '(Git Hg)
 inhibit-startup-message t
 select-enable-clipboard t
 make-backup-files nil
 column-number-mode t
 case-fold-search t
 current-language-environment "English"
 confirm-nonexistent-file-or-buffer nil
 compilation-window-height 10
 compilation-scroll-output 'first-error
 dabbrev-case-fold-search t
 save-abbrevs nil
 dired-listing-switches "-Al"
 save-interprogram-paste-before-kill t
 font-lock-maximum-decoration t
 vc-follow-symlinks t
 calendar-location-name "Cape Town"
 calendar-longitude 18.46
 calendar-latitude -33.98
 display-time-world-list
 '(("Africa/Johannesburg" "Cape Town")
   ("America/Los_Angeles" "San Francisco")
   ("Europe/London" "London")
   ("Europe/Berlin" "Berlin")))
;; Misc buffer settings
(setq-default
 fill-column 78
 tab-width 2
 indent-tabs-mode nil)
;; Disable annoying keys I accidently hit
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Hint that I use a dark background
;;(set-terminal-parameter nil 'background-mode 'dark)
;;(set-frame-parameter nil 'background-mode 'dark)

(display-time-mode 1)
(display-battery-mode 1)

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; go's present tool slides don't have a major mode for emacs, so ensure they
;; are opened as text files.
(add-to-list 'auto-mode-alist '("\\.slide\\'" . text-mode))

(use-package leuven-theme
  :config
  (enable-theme 'leuven))

(use-package avy
  :bind (("C-c SPC" . avy-goto-char)))

(require 'subr-x)

(use-package go-mode
  :config
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go test")))
  (add-hook 'go-mode-hook #'my-go-mode-hook)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setenv "GOROOT" (string-trim (shell-command-to-string "go env GOROOT")))
  (setenv "GOPATH" (string-trim (shell-command-to-string "go env GOPATH"))))

;; GO111MODULE=on go get github.com/davidrjenni/reftools/cmd/fillstruct
(use-package go-fill-struct
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-o f". go-fill-struct))
  :commands (go-fill-struct))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-j" . lsp-find-definition)
              ("C-c C-o d". lsp-describe-thing-at-point)
              ("C-c C-o h". lsp-symbol-highlight)
              ("C-c C-o j". lsp-goto-type-definition)
              ("C-c C-o r". lsp-find-references)
              ("C-c C-o s". counsel-imenu)
              ("C-c C-o x". lsp-rename)
              ("C-c C-o z". lsp-describe-session))
  :config

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq
   lsp-enable-file-watchers nil
   lsp-enable-snippet nil))

;; if you use company-mode for completion (otherwise, complete-at-point works out of the box):
(use-package company-lsp
  :commands company-lsp)

(use-package flycheck
  :init
  (global-flycheck-mode)
  ; elisp checking is annoying. checkdoc is targetted at elisp packages, I
  ; just hack at stuff.
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package ivy
  :diminish (ivy-mode . "")
  :bind (("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c k" . counsel-rg)
         ("C-x l" . counsel-locate))
  :config

  ;; Avoid long lines in counsel-rg
  (setq counsel-rg-base-command "rg -i -g '!vendor' --no-heading --line-number --color never --max-columns 200 %s ."))

(straight-register-package
 '(counsel-repo :host github :repo "keegancsmith/counsel-repo"))

(use-package counsel-repo
  :bind (("C-c j" . counsel-repo))
  :after counsel
  :config
  (setq
   counsel-repo-srcpaths '("~/go/src" "~/src" "~/.emacs.d/straight/repos")
   counsel-repo-action #'magit-status))

(defun my-org-link (url name)
  "Generate an org link using URL and NAME (title of page)."
  (cond
   ;; github issue/pr title
   ((string-match
     (rx (optional "(" (one-or-more digit) ") ")      ; notification count: eg "(1) "
         (submatch (+? anything))                     ; title: eg "my issue title"
         (optional " by " (one-or-more alphanumeric)) ; pr author: eg " by foo"
         " · "
         (or "Pull Request" "Issue")
         " #"
         (submatch (one-or-more digit))               ; PR/Issue ID
         " · "
         (submatch (one-or-more graphic))             ; owner: eg "microsoft"
         "/"
         (submatch (one-or-more graphic)))            ; repo:  eg "vscode"
     name)
    (let* ((title (match-string 1 name))
           (id    (match-string 2 name))
           (owner (match-string 3 name))
           (repo  (match-string 4 name))
           (desc  (cond ((equal repo "sourcegraph") "")
                        ((equal owner "sourcegraph") repo)
                        (t (format "%s/%s" owner repo)))))
      (format "%s [[%s][%s#%s]]" title url desc id)))
   ;; Sourcegraph RFC
   ((string-match
     (rx (submatch "RFC " (one-or-more digit)) ;; RFC number: eg "RFC 30"
         (optional " " (one-or-more letter))
         (optional " " (one-or-more letter))
         ": "
         (submatch (one-or-more anything)) ;; Title: eg "Zoekt Horizontal Scaling"
         " - Google Docs")
     name)
    (let ((id    (match-string 1 name))
          (title (match-string 2 name))
          (url   (replace-regexp-in-string "/edit.*" "" url)))
      (format "%s: %s [[%s][%s]]" id title url id)))
   ;; titles that look like jira issues
   ((string-match
     (rx "["
         (submatch (+? graphic))                        ; ticket
         "] "
         (submatch (+? anything))                       ; title
         " - Jira Service Desk")
     name)
    (let ((ticket (match-string 1 name))
          (title  (match-string 2 name)))
      (format "[[%s][{%s}]] %s" url ticket title)))
   ;; titles that look like "prefix: title"
   ((string-match
     (rx (submatch (+? graphic))                        ; prefix
         ": "
         (submatch (+? anything))                       ; title
         (optional " - " (one-or-more (not (any "-")))) ; suffix
         string-end)
     name)
    (let ((prefix (match-string 1 name))
          (title  (match-string 2 name)))
      (format "[[%s][%s]]: %s" url prefix title)))
   ;; no title (title is URL without scheme). Need to trim \u200 prefix
   ((string-suffix-p (string-trim name (rx nonascii)) url) url)
   ;; default
   (t (format "[[%s][%s]]" url name))))

;; (insert-before-markers "\"" (my-chrome-url) "\"\n\"" (my-chrome-name) "\"\n\"" (my-chrome-link) "\"")
(ert-deftest my-test-org-link ()
  "Tests the my-org-link."
  (should (equal (my-org-link
                  "https://docs.google.com/document/d/18w8T_KzYxQye8wg1g01QpMOX4_ERTtbOxMBRYaOEkmk/edit#heading=h.rf2d3v2oo71l"
                  "RFC 30: Zoekt Horizontal Scaling - Google Docs")
                 "RFC 30: Zoekt Horizontal Scaling [[https://docs.google.com/document/d/18w8T_KzYxQye8wg1g01QpMOX4_ERTtbOxMBRYaOEkmk][RFC 30]]"))
  (should (equal (my-org-link
                  "https://docs.google.com/document/d/redacted/edit"
                  "RFC 30 PRIVATE: redacted - Google Docs")
                 "RFC 30: redacted [[https://docs.google.com/document/d/redacted][RFC 30]]"))
  (should (equal (my-org-link
                  "https://docs.google.com/document/d/redacted/edit"
                  "RFC 60 REVIEW: Introduce a bi-weekly release cadence - Google Docs")
                 "RFC 60: Introduce a bi-weekly release cadence [[https://docs.google.com/document/d/redacted][RFC 60]]"))
  (should (equal (my-org-link
                  "https://github.com/sourcegraph/sourcegraph/issues/6031"
                  "Core Services: 3.10 tracking issue · Issue #6031 · sourcegraph/sourcegraph")
                 "Core Services: 3.10 tracking issue [[https://github.com/sourcegraph/sourcegraph/issues/6031][#6031]]"))
  (should (equal (my-org-link
                  "https://github.com/sourcegraph/zoekt/pull/10"
                  "index: Use roaring bitmaps for content posting lists by keegancsmith · Pull Request #10 · sourcegraph/zoekt")
                 "index: Use roaring bitmaps for content posting lists [[https://github.com/sourcegraph/zoekt/pull/10][zoekt#10]]"))
  (should (equal (my-org-link
                  "https://github.com/reviewdog/reviewdog"
                  "reviewdog/reviewdog: Automated code review tool integrated with any code analysis tools regardless of programming language")
                 "[[https://github.com/reviewdog/reviewdog][reviewdog/reviewdog]]: Automated code review tool integrated with any code analysis tools regardless of programming language"))
  (should (equal (my-org-link
                  "https://sourcegraph.atlassian.net/jira/servicedesk/projects/SG/queues/custom/1/SG-01"
                  "[SG-01] a jira title - Jira Service Desk")
                 "[[https://sourcegraph.atlassian.net/jira/servicedesk/projects/SG/queues/custom/1/SG-01][{SG-01}]] a jira title"))
  (should (equal (my-org-link
                  "http://db.csail.mit.edu/pubs/harizopoulosVLDB06.pdf"
                  "‎db.csail.mit.edu/pubs/harizopoulosVLDB06.pdf")
                 "http://db.csail.mit.edu/pubs/harizopoulosVLDB06.pdf"))
  (should (equal (my-org-link
                  "https://golang.org/"
                  "The Go Programming Language")
                 "[[https://golang.org/][The Go Programming Language]]")))

;(package-install (cadr (assq 'org package-archive-contents)))

(straight-use-package 'org-plus-contrib)

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-x C-j" . org-clock-goto))
  :config

  (defun org-insert-inactive-time-stamp ()
    "Insert an inactive time stamp."
    (interactive)
    (org-insert-time-stamp (current-time) t t))
  (define-key org-mode-map (kbd "C-c .") 'org-insert-inactive-time-stamp)

  ;; disable org-table-blank-field i'd rather have avy
  (define-key org-mode-map (kbd "C-c SPC") nil)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-mac-link)
  (add-to-list 'org-modules 'org-tempo)

  (defun my-org-clock-in-list ()
    "Calls org-clock-in with `\\[universal-argument]'."
    (interactive)
    (org-clock-in '(4)))
  (global-set-key (kbd "C-c i") 'my-org-clock-in-list)
  
  (require 'org-protocol)

  (defun my-safari-url ()
    (do-applescript "tell application \"Safari\" to get the URL of front document"))
  (defun my-safari-name ()
    (do-applescript "tell application \"Safari\" to get the name of front window"))
  (defun my-safari-link ()
    (my-org-link (my-safari-url) (my-safari-name)))
  (defun my-chrome-url ()
    (do-applescript "tell application \"Google Chrome\" to get URL of active tab of first window"))
  (defun my-chrome-name ()
    (do-applescript "tell application \"Google Chrome\" to get name of active tab of first window"))
  (defun my-chrome-link ()
    (my-org-link (my-chrome-url) (my-chrome-name)))
  (defun my-browser-link ()
    (if (equal 0 (call-process "pgrep" nil nil nil "-q" "^Google Chrome$"))
        (my-chrome-link)
      (my-safari-link)))
  (defun my-insert-link ()
    (interactive)
    (insert-before-markers (my-browser-link)))

  (require 'ob-shell)
  (require 'ob-awk)

  (setq
   org-agenda-files '("~/org-files")
   org-refile-targets '((("~/org-files/work.org" "~/org-files/home.org" "~/org-files/backlog.org" "~/org-files/notes.org") :maxlevel . 1))
   org-archive-location "%s_archive::datetree/"
   org-deadline-warning-days 14
   org-default-notes-file "~/org-files/inbox.org"
   org-reverse-note-order t
   org-fast-tag-selection-single-key 'expert
   org-export-backends '(ascii html icalendar md)
   ;; prevent foo_bar being exported as foo<sub>bar</sub>. Requires foo_{bar}
   ;; for subscript.
   org-export-with-sub-superscripts '{}
   org-agenda-span 'day
   org-agenda-dim-blocked-tasks nil
   org-adapt-indentation 'headline-data
   org-catch-invisible-edits 'show-and-error
   org-hide-leading-stars t
   org-enforce-todo-dependencies t
   org-log-done 'time
   org-log-redeadline 'time
   org-log-reschedule 'time
   org-babel-load-languages '((emacs-lisp . t) (sh . t) (awk . t))
   ;org-confirm-babel-evaluate nil
   org-capture-templates
   '(("c" "Task" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U\n%a")
     ("b" "Browser" entry (file "~/org-files/inbox.org")
      "* TODO %(my-browser-link)\n%U")
     ("n" "Note" entry (file "~/org-files/notes.org")
      "* %?\n  %U\n%a")
     ("d" "Daily Checklist" entry (file+olp+datetree "~/org-files/journal.org")
      (file "~/org-files/checklist.txt"))
     ("a" "Appointment today" entry (file+olp+datetree "~/org-files/meetings.org")
        "* %?\n  %^t" :tree-type week)
     ("o" "P0 ops work scheduled and clocked in now" entry (file+headline "~/org-files/work.org" "Ops")
      "* P0 Ops :urgent:ops:\n  %t\n  %u" :clock-in t :clock-keep t :empty-lines 1)
     ("m" "Meeting now" entry (file+olp+datetree "~/org-files/meetings.org")
      "* %? :meeting:\n  %T" :clock-in t :clock-keep t :jump-to-captured t :empty-lines 1 :tree-type week)
     ("j" "Journal" entry (file+olp+datetree "~/org-files/journal.org")
      "* %^{title} %^g\n    %U\n\n%?")
     ("p" "" entry (file "~/org-files/inbox.org")
      "* TODO %:description\n%U\n%:link\n\n#+BEGIN_QUOTE\n%:initial\n#+END_QUOTE" :immediate-finish t :jump-to-captured t)
     ("L" "" entry (file "~/org-files/inbox.org")
      "* TODO %:description\n%U\n%:link" :immediate-finish t :jump-to-captured t))))

(use-package org-pomodoro
  :after org)

(use-package ox-gfm
  :after org)

(use-package ob-go
  :after org)

; Can do :async on src code blocks
(use-package ob-async
  :after org)

; Highlight code when exporting
(use-package htmlize)

(use-package direnv
 :config
 (direnv-mode))

(use-package python
  :config
  (setq
   python-shell-interpreter "python3"
   python-shell-completion-native-enable nil)
  (use-package blacken))

;; (use-package paredit
;;   :diminish paredit-mode
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'paredit-mode))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (defun my-git-commit-mode-hook ()
    "sets fill-column to the suggested git convention."
    (setq fill-column 72))
  (add-hook 'git-commit-mode-hook #'my-git-commit-mode-hook))

(use-package git-timemachine)

(use-package browse-at-remote)

(use-package minions
  :config (minions-mode 1))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/idle-delay 0.5)
  (guide-key-mode 1))

;; For visual wrapping at 80 columns when editing markdown.
(use-package visual-fill-column)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (defun my-gfm-mode-hook ()
    (visual-line-mode)
    (visual-fill-column-mode)
    (setq word-wrap t))
  (add-hook 'gfm-mode-hook #'my-gfm-mode-hook))

;; https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
(use-package edit-server
  :config
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))
        edit-server-new-frame nil)
  (edit-server-start))

(use-package unfill
  :bind ("M-Q" . unfill-paragraph))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(straight-use-package
 '(promql-mode :host github :repo "Andor/promql-mode"))

(use-package link-hint
  :bind
  ("C-c o" . link-hint-open-link))

(use-package notmuch)

(use-package gnus
  :bind (("C-c n" . gnus))
  :init
  (setq
   gnus-select-method '(nntp "news.gmane.io")
   nndraft-directory "~/.news/drafts"))

;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
(use-package elfeed)

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org))

(use-package youtube-dl
  :after elfeed
  :config

  (require 'cl-lib)

  (defun elfeed-show-youtube-dl ()
    "Download the current entry with youtube-dl."
    (interactive)
    (pop-to-buffer (youtube-dl (elfeed-entry-link elfeed-show-entry))))

  (cl-defun elfeed-search-youtube-dl (&key slow)
    "Download the current entry with youtube-dl."
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (if (null (youtube-dl (elfeed-entry-link entry)
                              :title (elfeed-entry-title entry)
                              :slow slow))
            (message "Entry is not a YouTube link!")
          (message "Downloading %s" (elfeed-entry-title entry)))
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless (use-region-p) (forward-line)))))

  (define-key elfeed-show-mode-map "d" 'elfeed-show-youtube-dl)
  (define-key elfeed-search-mode-map "d" 'elfeed-search-youtube-dl)
  (define-key elfeed-search-mode-map "L" 'youtube-dl-list)

  (setq
   youtube-dl-arguments (list "--no-mtime" "--restrict-filenames" "-f" "[height<=?720]/best")
   youtube-dl-directory "~/youtube/feed"))
(use-package ledger-mode)

(use-package rg
  :config
  (setq
   rg-group-result t
   rg-hide-command t
   rg-show-columns nil
   rg-show-header t
   rg-custom-type-aliases nil
   rg-default-alias-fallback "all")

  :bind (("M-s g" . rg)
         :map rg-mode-map
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

(setq browse-url-browser-function
      '(
        ;; Open youtube links directly in mpv
        ("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
        ;; Otherwise open URLs without taking focus from emacs
        ("." . browse-url-background)))

(defun browse-url-mpv (url &optional single)
  (let ((process-connection-type nil))
        (start-process "mpv" "mpv" "mpv" url)))

(defun browse-url-background (url &optional single)
  (start-process "open" nil "open" "-g" url))

(defun load-file-exists (file)
  "Load the Lisp file named FILE if it exists."
  (if (file-exists-p file)
    (load-file file)))

(load-file "~/.emacs.d/revbufs.el")

(setq custom-file "~/.emacs.d/custom.el")
(load-file-exists custom-file)

;;; init.el ends here
