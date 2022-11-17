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
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

(when (display-graphic-p)
  (set-frame-font "Iosevka" nil t)
  (set-face-attribute 'default nil :font "Iosevka" :height 120)
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8"))

(require 'server)
(unless (server-running-p)
  (server-start))

;; General config
(setq user-full-name "Keegan Carruthers-Smith")
(setq user-mail-address "keegan.csmith@gmail.com")
;; Highlight matching paren
(show-paren-mode 1)
;; Yes-or-No queries become Y-or-N
(fset 'yes-or-no-p 'y-or-n-p)
;; Better buffer names when names conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix nil)
;; Misc settings
(setq
 vc-handled-backends '(Git Hg)
 select-enable-clipboard t
 make-backup-files nil
 column-number-mode t
 case-fold-search t
 current-language-environment "English"
 confirm-nonexistent-file-or-buffer nil
 compilation-window-height 10
 save-abbrevs nil
 dired-listing-switches "-Al"
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil
 font-lock-maximum-decoration t
 vc-follow-symlinks t)
(setq-default
 show-trailing-whitespace t
 indicate-empty-lines t
 indicate-buffer-boundaries 'left)
(use-package emacs
  :custom
  (dabbrev-case-fold-search t)
  (epg-pinentry-mode 'loopback)
  (compilation-scroll-output 'first-error)
  (tramp-default-method "sshx")
  (ispell-dictionary "british")
  (warning-minimum-level :error)
  (world-clock-list '(("Africa/Johannesburg" "Cape Town")
   ("America/Los_Angeles" "San Francisco")
   ("Europe/London" "London")
   ("Europe/Berlin" "Berlin")))
  :config
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))
(use-package calendar
  :custom
  (calendar-date-style 'iso)
  (calendar-location-name "Cape Town")
  (calendar-longitude 18.46)
  (calendar-latitude -33.98))
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

(defun my/stefan ()
  ""
  (interactive)
  (insert "Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>"))

;; isearch whitespaces is means .*
(setq
 search-whitespace-regexp ".*?"
 isearch-lax-whitespace t
 isearch-regexp-lax-whitespace nil)

;; Hint that I use a dark background
;;(set-terminal-parameter nil 'background-mode 'dark)
;;(set-frame-parameter nil 'background-mode 'dark)

(display-time-mode 1)

(if (not (string= (system-name) "habitat"))
  (display-battery-mode 1))

(global-set-key (kbd "C-c C-c") 'compile)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; go's present tool slides don't have a major mode for emacs, so ensure they
;; are opened as text files.
(add-to-list 'auto-mode-alist '("\\.slide\\'" . text-mode))

(use-package leuven-theme
  :if (display-graphic-p)
  :config
  (enable-theme 'leuven))

(use-package modus-themes
  :if (not (display-graphic-p))
  :init
  (load-theme 'modus-vivendi t t)
  :config
  (enable-theme 'modus-vivendi))

(use-package avy
  :bind (("C-c SPC" . avy-goto-word-1)))

(require 'subr-x)

(use-package exec-path-from-shell
  :if (display-graphic-p)
  :defer 1
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOROOT" "SRCPATH"))
  :config
  (exec-path-from-shell-initialize))

(use-package savehist
  :unless noninteractive
  :config
  (savehist-mode 1))

(use-package saveplace
  :unless noninteractive
  :config
  (save-place-mode 1))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :config
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go test")))
  (add-hook 'go-mode-hook #'my-go-mode-hook)
  (setq gofmt-command "goimports")
  (setenv "GOPATH" (string-trim (shell-command-to-string "go env GOPATH"))))

;; GO111MODULE=on go get github.com/davidrjenni/reftools/cmd/fillstruct
(use-package go-fill-struct
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-o f" . go-fill-struct))
  :commands (go-fill-struct))

(use-package company
  :config
  (global-company-mode))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  (go-mode . eglot-ensure))

;; lsp-mode performance tuning [[file:straight/repos/lsp-mode/docs/page/performance.md]]
(use-package emacs
  :config

  (setq read-process-output-max (* 1024 1024))) ;; 1mb

(use-package flycheck
  :init
  (global-flycheck-mode)
  ; elisp checking is annoying. checkdoc is targetted at elisp packages, I
  ; just hack at stuff.
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(straight-register-package
 '(vertico :host github :repo "minad/vertico"))

(use-package vertico
  :init
  (vertico-mode))

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations for completing-read
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

;; consult version of my counsel-repo elisp.
(defun consult-project ()
  "jump to a repo"
  (interactive)
  (let* ((selectrum-should-sort nil)
         (dirs (or (parse-colon-path (getenv "SRCPATH"))
                   '("~/src")))
         (cmd (string-join (cons "~/go/bin/counsel-repo" dirs) " "))
         (cands (split-string (shell-command-to-string cmd)))
         (repo (completing-read "Find repo: " cands nil t)))
    (magit-status (car
                   (seq-filter #'file-directory-p
                               (mapcar
                                (lambda (dir) (expand-file-name (concat dir "/" repo)))
                                dirs))))))

;; consult version of counsel-git. Faster than using consult-find. No need for
;; the fancy async stuff for the repos I work on. Probably could configure
;; consult-find-command instead.
(defun consult-git ()
  "Find file in the current Git repository."
  (interactive)
  (let* ((default-directory (funcall consult-project-function nil))
         (cmd "git ls-files -z --full-name --")
         (cands (split-string (shell-command-to-string cmd) "\0" t))
         (file (completing-read "Find file: " cands nil t)))
    (find-file file)))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("C-c k" . consult-ripgrep)
         ("C-c g" . consult-git)

         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)

         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s p" . consult-project)

         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-s e" . consult-isearch)
         ("M-s l" . consult-line))

  :custom
  ;; smaller max-columns and smart-case
  (consult-ripgrep-command "rg --null --line-buffered --color=never --max-columns=200 --no-heading --smart-case --line-number . -e ARG OPTS")

  :config

  ;; disable automatic preview when it loads a file from a search due to it
  ;; being slow.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h C" . helpful-command)))

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

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-x C-j" . org-clock-goto))
  :config

  ;; use my fork of ox-gfm which supports backticks in example export.
  (straight-use-package
   '(ox-gfm :type git :host github :repo "keegancsmith/ox-gfm"))
  (require 'ox-gfm)

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
    (if (equal 0 (call-process "pgrep" nil nil nil "-q" "^Safari$"))
        (my-safari-link)
      (my-chrome-link)))
  (defun my-insert-link ()
    (interactive)
    (insert-before-markers (my-browser-link)))

  (defun my-org-pull ()
    "Updates org-files from git remote."
    (interactive)
    (org-save-all-org-buffers)
    (let ((default-directory "~/org-files/"))
      (magit-pull-from-pushremote '()))
    (revbufs))

  (require 'ob-shell)
  (require 'ob-awk)
  (require 'ob-python)

  (setq
   current-journal-filename "~/org-files/journals/2022/2022-11-Nov.org"
   org-agenda-files '("~/org-files"
                      "~/org-files/journals/2021"
                      "~/org-files/journals/2022")
   org-refile-targets `((("~/org-files/work.org" "~/org-files/home.org" "~/org-files/backlog.org" "~/org-files/notes.org" "~/org-files/learn.org") :maxlevel . 1)
                        ((,current-journal-filename) :maxlevel . 1))
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
   org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@)" "CANCELLED(c@)"))
   org-babel-load-languages '((emacs-lisp . t) (shell . t) (awk . t) (python . t))
   org-babel-python-command "python3"
   ;org-confirm-babel-evaluate nil
   org-capture-templates
   '(("c" "Task" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U")
     ("C" "Task with context" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U\n%a")
     ("b" "Browser" entry (file "~/org-files/inbox.org")
      "* TODO %(my-browser-link)\n%U")
     ("w" "Week Plan" entry (file+olp+datetree current-journal-filename)
      (file "~/org-files/week-plan.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
     ("d" "Day Plan" entry (file+olp+datetree current-journal-filename)
      (file "~/org-files/plan.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
     ("e" "End of day" entry (file+olp+datetree current-journal-filename)
      (file "~/org-files/eod.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
     ("o" "P0 ops work scheduled and clocked in now" entry (file+headline "~/org-files/work.org" "Ops")
      "* P0 Ops :urgent:ops:\n  %t\n  %u" :clock-in t :clock-keep t :empty-lines 1)
     ("j" "Journal" entry (file+olp+datetree current-journal-filename)
      "* %? %U\n" :empty-lines 1)
     ("J" "Journal HERE" entry (file+olp+datetree (lambda () (org-capture-get :original-file)))
      "* %? %U\n" :empty-lines 1))))

(use-package org-contrib
  :after org)

(use-package ob-go
  :after org)

; Can do :async on src code blocks
(use-package ob-async
  :after org)

; Highlight code when exporting
(use-package htmlize)

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(
          ("mp4" . "mpv")
          ("mkv" . "mpv")
          )))

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

;; from https://www.bennee.com/~alex/blog/2020/12/14/magit-file-dispatch/
(defun my-magit-dispatch (&optional prefix)
  (interactive "P")
  (if (or prefix
          (not (buffer-file-name))
          (not (functionp 'magit-file-dispatch)))
      (magit-status)
    (magit-file-dispatch)))

(use-package magit
  :bind (("C-x g" . my-magit-dispatch))
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
         ("qutebrowser-editor-[^/]+\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (defun my-gfm-mode-hook ()
    (visual-line-mode)
    (visual-fill-column-mode)
    (flyspell-mode)
    (setq word-wrap t))
  (add-hook 'gfm-mode-hook #'my-gfm-mode-hook))

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

(use-package notmuch
  :commands (notmuch)
  :bind (("C-c m" . notmuch))
  :if (string= (system-name) "real.local"))

;; org notmuch links
(use-package ol-notmuch
  :after notmuch)

(use-package messages-are-flowing
  :config
  (add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines))

(use-package gnus
  :init
  (setq
   gnus-select-method '(nntp "news.gmane.io")
   nndraft-directory "~/.news/drafts"))

;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
(use-package elfeed
  :commands (elfeed))

(use-package elfeed-org
  :after elfeed
  :config
  (elfeed-org))

(use-package youtube-dl
  :after elfeed
  :custom
  (youtube-dl-program "yt-dlp")
  (youtube-dl-arguments (list "--no-colors" "--no-mtime" "--restrict-filenames" "-f" "[height<=?720]/best"))
  (youtube-dl-directory "~/youtube/feed")

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
  (define-key elfeed-search-mode-map "L" 'youtube-dl-list))

(use-package pass)

(use-package auth-source-pass
  :after pass
  :config
  (auth-source-pass-enable))

(use-package ledger-mode
  :commands (ledger-mode)
  :mode (("\\.ledger\\'" . ledger-mode)))

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
         ("M-s r" . rg-dwim-project-dir)
         :map rg-mode-map
         ("C-n" . next-line)
         ("C-p" . previous-line)
         ("M-n" . rg-next-file)
         ("M-p" . rg-prev-file)))

(use-package elpher)

(setq browse-url-handlers
      '(
        ;; Open youtube links directly in mpv
        ("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
        ;; Otherwise open URLs without taking focus from emacs
        ("." . browse-url-background)))

(defun browse-url-mpv (url &optional single)
  (let ((process-connection-type nil))
        (start-process "mpv" "mpv" "mpv" url)))

(defun browse-url-background (url &optional single)
  (if (string= system-type "darwin")
      (start-process "open" nil "open" "-g" url)
    (start-process "qutebrowser" nil "qutebrowser" url)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package nix-mode)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package zig-mode)

(use-package bongo
  :custom
  (bongo-custom-backend-matchers '((mpv (local-file) "opus")))
  (bongo-default-directory "~/youtube/feed/music")
  :config
  (add-to-list 'bongo-audio-file-name-extensions "opus")
  :commands (bongo))

;; trying out clojure
(progn
  (use-package rainbow-delimiters)

  (use-package clojure-mode
    :ensure t
    :config
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

  (use-package inf-clojure
    :ensure t
    :config
    (add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode))

  (use-package cider
    :ensure t
    :config
    (setq nrepl-log-messages t)
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)))

(defun load-file-exists (file)
  "Load the Lisp file named FILE if it exists."
  (if (file-exists-p file)
    (load-file file)))

(load-file "~/.emacs.d/revbufs.el")

(setq custom-file "~/.emacs.d/custom.el")
(load-file-exists custom-file)

;;; init.el ends here
