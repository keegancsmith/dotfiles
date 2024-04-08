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
  (set-frame-font
   (pcase (system-name)
     ("fa.local" "JetBrains Mono Light 14")
     (_          "JetBrains Mono Light 12"))
   nil t)
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_ALL" "en_US.UTF-8"))

;; use cocoa bindings since that is what I am used to.
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

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
 dired-listing-switches "-Alh"
 save-interprogram-paste-before-kill t
 sentence-end-double-space nil
 font-lock-maximum-decoration t
 vc-follow-symlinks t)
(use-package emacs
  :custom
  (dabbrev-case-fold-search t)
  (epg-pinentry-mode 'loopback)
  (compilation-scroll-output 'first-error)
  (tramp-default-method "sshx")
  (image-converter 'imagemagick) ; prefer over ffmpeg
  (image-use-external-converter t)
  (ispell-dictionary "british")
  (set-mark-command-repeat-pop t)
  (warning-minimum-level :error)
  (world-clock-list '(("Africa/Johannesburg" "Cape Town")
   ("America/Los_Angeles" "San Francisco")
   ("Europe/London" "London")
   ("Europe/Berlin" "Berlin")))
  (warning-suppress-log-types '((comp) (comp)))
  (warning-suppress-types '((comp)))
  :config
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  (recentf-mode 1))
(use-package calendar
  :custom
  (calendar-date-style 'iso)
  (calendar-location-name "Cape Town")
  (calendar-longitude 18.46)
  (calendar-latitude -33.98)
  ;; Republic of South Africa's National Holidays.
  ;; https://www.emacswiki.org/emacs/CalendarLocalization#h5o-52
  (holiday-local-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-fixed 3 21 "Human Rights Day")
     (holiday-easter-etc -2 "Good Friday")
     (holiday-easter-etc +1 "Family Day")
     (holiday-fixed 4 27 "Freedom Day")
     (holiday-fixed 5 1 "Workers' Day")
     (holiday-fixed 6 16 "Youth Day")
     (holiday-fixed 8 9 "National Women's Day")
     (holiday-fixed 9 24 "Heritage Day")
     (holiday-fixed 12 16 "Day of Reconciliation")
     (holiday-fixed 12 25 "Christmas Day")
     (holiday-fixed 12 26 "Day of Goodwill"))))
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

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun my/stefan ()
  ""
  (interactive)
  (insert "Co-authored-by: Stefan Hengl <stefan@sourcegraph.com>"))

;; isearch whitespaces means .*
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

;; GC hacks
(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package ef-themes
  :config
  ;; Disable all other themes to avoid awkward blending
  (mapc #'disable-theme custom-enabled-themes)
  (ef-themes-select 'ef-spring))

(use-package avy
  :bind (("C-c SPC" . avy-goto-word-1)))

(use-package ace-window
  :bind ("C-x o" . ace-window))

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
  :custom
  (savehist-additional-variables '(register-alist kill-ring))
  :config
  (savehist-mode 1))

(use-package saveplace
  :unless noninteractive
  :config
  (save-place-mode 1))

;; Allow list for using showing whitespace. Too many modes I don't want it to
;; do globally.
(progn
  (defun show-trailing-whitespace-hook ()
    (setq show-trailing-whitespace t
          indicate-empty-lines t
          indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'show-trailing-whitespace-hook)
  (add-hook 'org-mode-hook #'show-trailing-whitespace-hook))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :config
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go test")))
  (add-hook 'go-mode-hook #'my-go-mode-hook)
  (add-hook 'go-ts-mode-hook #'my-go-mode-hook)
  (setq gofmt-command (expand-file-name "~/go/bin/goimports"))
  (setenv "GOPATH" (string-trim (shell-command-to-string "go env GOPATH")))
  (when (bound-and-true-p consult-imenu-config)
    (add-to-list 'consult-imenu-config '(go-mode :toplevel "Function"
                                                 :types ((?s "Struct"    font-lock-type-face)
                                                         (?i "Interface" font-lock-type-face)
                                                         (?f "Function"  font-lock-function-name-face)
                                                         (?m "Method"    font-lock-function-name-face)
                                                         (?o "Field"     font-lock-variable-name-face)
                                                         (?c "Constant"  font-lock-constant-face)
                                                         (?v "Variable"  font-lock-variable-name-face))))))

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
  (go-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (zig-mode . eglot-ensure))

;; lsp-mode performance tuning [[file:straight/repos/lsp-mode/docs/page/performance.md]]
(use-package emacs
  :config

  (setq read-process-output-max (* 1024 1024))) ;; 1mb

(use-package typescript-mode)

(use-package flycheck
  :init
  (global-flycheck-mode)
  ; elisp checking is annoying. checkdoc is targetted at elisp packages, I
  ; just hack at stuff.
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; weird workaround. On latest vertico it fails since it can't find compat
;; library. But just requiring it seems to sort it out.
(use-package compat)

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

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; consult version of my counsel-repo elisp.
(defun consult-project ()
  "jump to a repo"
  (interactive)
  (let* ((selectrum-should-sort nil)
         (dirs (or (parse-colon-path (getenv "SRCPATH"))
                   '("~/src")))
         (cmd (string-join (cons "counsel-repo -verbose" dirs) " "))
         (cands (mapcar (lambda (line) (butlast (split-string line)))
                        (split-string (shell-command-to-string cmd) "\n")))
         (repo (completing-read "Find repo: " cands nil t))
         (path (cadr (assoc repo cands))))
    (magit-status path)))

;; consult version of counsel-git. Faster than using consult-find. No need for
;; the fancy async stuff for the repos I work on. Probably could configure
;; consult-find-command instead.
(defun consult-git ()
  "Find file in the current Git repository."
  (interactive)
  (let* ((default-directory (project-root (project-current)))
         (cmd "git ls-files -z --full-name --")
         (cands (split-string (shell-command-to-string cmd) "\0" t))
         (file (completing-read "Find file: " cands nil t)))
    (find-file file)))

(use-package consult
  :demand t
  :bind (("M-y" . consult-yank-pop)
         ("C-c k" . consult-ripgrep)
         ("C-c g" . consult-git)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)  ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)

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
   :preview-key "M-.")

  (setq register-preview-delay 0.8
        register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

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
   ;; Other Google Doc links
   ((string-match
     (rx (submatch (one-or-more anything)) ;; Title: eg "Zoekt Horizontal Scaling"
         " - Google Docs")
     name)
    (let ((title (match-string 1 name))
          (url   (replace-regexp-in-string "/edit.*" "" url)))
      (format "[[%s][%s]]" url title)))
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

  (use-package ox-gfm)
  (use-package ox-slack)

  ;; For some reason "C-c !" doesn't work, so rebind to a key similiar to
  ;; org-time-stamp
  (define-key org-mode-map (kbd "C-c C-.") 'org-time-stamp-inactive)

  ;; disable org-table-blank-field i'd rather have avy
  (define-key org-mode-map (kbd "C-c SPC") nil)

  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-tempo)

  (defun my-org-clock-in-list ()
    "Calls org-clock-in with `\\[universal-argument]'."
    (interactive)
    (org-clock-in '(4)))
  (global-set-key (kbd "C-c i") 'my-org-clock-in-list)

  (require 'org-protocol)

  (defun my-pgrep-exact? (name)
    (equal 0 (call-process "pgrep" nil nil nil "-q" "-x" name)))
  (defun my-safari-url ()
    (json-read-from-string
     (do-applescript "tell application \"Safari\" to get the URL of front document")))
  (defun my-safari-name ()
    (json-read-from-string
     (do-applescript "tell application \"Safari\" to get the name of front window")))
  (defun my-safari-link ()
    (my-org-link (my-safari-url) (my-safari-name)))
  (defun my-chrome-url ()
    (json-read-from-string
     (do-applescript "tell application \"Google Chrome\" to get URL of active tab of first window")))
  (defun my-chrome-name ()
    (json-read-from-string
     (do-applescript "tell application \"Google Chrome\" to get name of active tab of first window")))
  (defun my-chrome-link ()
    (my-org-link (my-chrome-url) (my-chrome-name)))
  (defun my-browser-link ()
    (cond
     ((my-pgrep-exact? "Google Chrome") (my-chrome-link))
     ((my-pgrep-exact? "Safari") (my-safari-link))))
  (defun my-insert-link ()
    (interactive)
    (insert-before-markers (my-browser-link)))

  (require 'ob-shell)
  (require 'ob-awk)
  (require 'ob-python)

  ;; decoded-time-add doesn't seem to work across year boundaries when month
  ;; is negative. So gotta implement myself.
  (defun my-decoded-time-add-month (time month-delta)
    "adds month-delta to time. Will always set day to 15 for edge cases."
    (let ((time (copy-sequence time)))
      (setf (decoded-time-day time) 15)
      (setf (decoded-time-month time) (+ (decoded-time-month time) month-delta))

      (while (<= (decoded-time-month time) 0)
        (setf (decoded-time-year time) (1- (decoded-time-year time)))
        (setf (decoded-time-month time) (+ (decoded-time-month time) 12)))

      (while (> (decoded-time-month time) 12)
        (setf (decoded-time-year time) (1+ (decoded-time-year time)))
        (setf (decoded-time-month time) (- (decoded-time-month time) 12)))

      time))

  ;; Dynamically calculate the list of journals to include. I normally include
  ;; the previous 2 months and 1 month in the future.
  (require 'time-date)
  (let* ((from-month-delta -2)
         (to-month-delta    1)
         (time (decode-time (current-time)))
         (month (decoded-time-month time))
         active-projects agenda-journals refile-journals current-journal)

    (cl-flet ((journals (from-month-delta to-month-delta)
                (mapcar (lambda (month-delta)
                             (format-time-string "~/org-files/journals/%Y/%Y-%m-%b.org"
                                                 (encode-time
                                                  (my-decoded-time-add-month time month-delta))))
                        (number-sequence from-month-delta to-month-delta))))

      (setq agenda-journals (journals -2 1)
            refile-journals (journals 0 1)
            current-journal (car refile-journals)))

    ;; Agenda only wants files that exist
    (setq agenda-journals (seq-filter #'file-exists-p agenda-journals))

    (setq
     active-projects '("~/org-files/work/projects/2024/noodle/noodle.org"
                       "~/org-files/work/projects/2023/keyword/keyword.org")
     org-agenda-files (append '("~/org-files/inbox.org") active-projects agenda-journals)
     org-refile-targets `((("~/org-files/work.org" "~/org-files/home.org" "~/org-files/backlog.org" "~/org-files/notes.org" "~/org-files/learn.org") :maxlevel . 1)
                          (,active-projects :level . 1)
                          (,refile-journals :level . 1))
     org-capture-templates
     `(("c" "Task" entry (file "~/org-files/inbox.org")
        "* TODO %?\n  %U")
       ("C" "Task with context" entry (file "~/org-files/inbox.org")
        "* TODO %?\n  %U\n%a")
       ("b" "Browser" entry (file "~/org-files/inbox.org")
        "* TODO %(my-browser-link)\n%U")
       ("w" "Week Plan" entry (file+olp+datetree ,current-journal)
        (file "~/org-files/week-plan.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
       ("d" "Day Plan" entry (file+olp+datetree ,current-journal)
        (file "~/org-files/plan.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
       ("e" "End of day" entry (file+olp+datetree ,current-journal)
        (file "~/org-files/eod.txt") :clock-in t :clock-keep t :immediate-finish t :jump-to-captured t)
       ("o" "P0 ops work scheduled and clocked in now" entry (file+headline "~/org-files/work.org" "Ops")
        "* P0 Ops :urgent:ops:\n  %t\n  %u" :clock-in t :clock-keep t :empty-lines 1)
       ("j" "Journal" entry (file+olp+datetree ,current-journal)
        "* %? %U\n" :empty-lines 1)
       ("J" "Journal HERE" entry (file+olp+datetree (lambda () (org-capture-get :original-file)))
        "* %? %U\n" :empty-lines 1))))

  ;; Trying out estimates again
  (setq
   org-global-properties '(("Effort_ALL" . "0 0:10 0:20 0:30 1:00 2:00"))
   org-columns-default-format "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
   org-columns-default-format-for-agenda "%10CATEGORY %40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
   org-agenda-columns-add-appointments-to-effort-sum t)

  (setq
   org-refile-use-outline-path 'file
   org-outline-path-complete-in-steps nil
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
   org-html-validation-link nil
   org-enforce-todo-dependencies t
   org-log-done 'time
   org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@)" "CANCELLED(c@)"))
   ;org-confirm-babel-evaluate nil
   org-babel-load-languages '((emacs-lisp . t) (shell . t) (awk . t) (python . t))
   org-babel-python-command "python3"))

(use-package org-contrib
  :after org)

(use-package ob-go
  :after org)

; Can do :async on src code blocks
(use-package ob-async
  :after org)

;; https://www.reddit.com/r/orgmode/comments/oiui2v/comment/h503cbg/
(defun hr/paste-html-to-org ()
  "Takes the contents of the system clip/paste-board, and uses
`pandoc' to convert it to the org-mode format."
  (interactive)
  (let* ((clip (if (eq system-type 'darwin)
                   "pbv public.html"
                 "xclip -out -selection 'clipboard' -t text/html"))
         (format (if (eq major-mode 'org-mode) "org" "markdown"))
         (pandoc (concat "pandoc -f html -t " format))
         (cmd    (concat clip " | " pandoc))
         (text   (shell-command-to-string cmd)))
    (kill-new text)
    (yank)))

; Highlight code when exporting
(use-package htmlize)

(use-package dired-open
  :custom
  (dired-open-extensions
   '(
     ("mp4" . "mpv")
     ("mkv" . "mpv")
     ("webm" . "mpv")
     )))

(use-package direnv
 :config
 (direnv-mode))

(use-package lua-mode)

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

(defun my-magit-clone-default-directory (repo)
  "Lay out clones under ~/src."
  (if (string-match
       (rx "git@github.com:"
           (submatch (+? anything) "/")
           (+? anything))
       repo)
      (expand-file-name (concat "~/src/github.com/" (match-string 1 repo)))
    default-directory))

(use-package magit
  :bind (("C-x g" . my-magit-dispatch))
  :custom
  (magit-auto-revert-mode nil)
  (magit-clone-default-directory #'my-magit-clone-default-directory)
  :config
  (defun my-git-commit-mode-hook ()
    "sets fill-column to the suggested git convention."
    (setq fill-column 72))
  (add-hook 'git-commit-mode-hook #'my-git-commit-mode-hook))

;; recipe still points to gitlab for git-timemachine
(straight-use-package
 '(git-timemachine :host codeberg :repo "pidu/git-timemachine"))

(use-package browse-at-remote)

(use-package minions
  :config (minions-mode 1))

(use-package dockerfile-mode)

(use-package yaml-mode)

(use-package guide-key
  :custom
  (guide-key/guide-key-sequence t)
  (guide-key/polling-time 0.5)
  :config
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
  ("C-c o" . my-link-hint-open-link)
  :commands (link-hint-open-link link-hint-copy-link)
  :init
  (defun my-link-hint-open-link ()
    "use link-hint-copy-link with prefix-arg otherwise link-hint-open-link"
    (interactive)
    (if current-prefix-arg
        (link-hint-copy-link)
      (link-hint-open-link))))

(defmacro defun-shell (name command)
  `(defun ,name ()
     ""
     (interactive)
     (let ((shell-command-buffer-name-async (concat "*" (symbol-name ',name) "*")))
       (async-shell-command ,command))))

;; only install notmuch on work laptop
(when (string= (system-name) "fa.local")
  (bind-key "C-c m n" (defun-shell notmuch-new         "notmuch new"))
  (bind-key "C-c m g" (defun-shell notmuch-github      "notmuch github | xargs open"))
  (bind-key "C-c m d" (defun-shell notmuch-github-done "notmuch github done"))

  (use-package notmuch
    :commands (notmuch)
    :bind (("C-c m m" . notmuch)))

  ;; org notmuch links
  (use-package ol-notmuch
    :after notmuch))

;; ssh into work machine to run mail commands
(when (string= (system-name) "habitat")
  (bind-key "C-c m m" (defun-shell notmuch             "kitty ssh -t fa.local emacsclient -nw --eval '\\(notmuch\\)'"))
  (bind-key "C-c m n" (defun-shell notmuch-new         "ssh fa.local notmuch new"))
  (bind-key "C-c m g" (defun-shell notmuch-github      "ssh fa.local notmuch github | xargs qutebrowser"))
  (bind-key "C-c m d" (defun-shell notmuch-github-done "ssh fa.local notmuch github done")))

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
  (youtube-dl-directory (expand-file-name "~/youtube/feed"))

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

(use-package erc
  :ensure nil
  :demand t
  :custom
  (erc-autojoin-channels-alist '((Libera.Chat "#nix-darwin" "#zig" "#emacs" "#nixos" "#notmuch" "#qutebrowser")))
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'bury)
  :config
  (defun my-libera-erc ()
    "Connect to libera.chat"
    (interactive)
    (let* ((server "irc.libera.chat")
           (found (seq-first (auth-source-search
                              :host server
                              :max-tokens 1
                              :require '(:user :secret)))))
      (erc-tls :server server
               :port "6697"
               :nick (plist-get found :user)
               :password (auth-info-password found)))))

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

;; I always open URLs without taking focus from emacs.
(setq browse-url-handlers
      '(
        ;; Youtube works better in Chrome
        ("https:\\/\\/www\\.youtu\\.*be." . browse-url-background-chrome)
        ;; Work URLs which require Okta (so Chrome)
        ("https:\\/\\/docs\\.google" . browse-url-background-chrome)
        ("https:\\/\\/sourcegraph\\.slack" . browse-url-background-chrome)
        ("https:\\/\\/console\\.cloud\\.google\\.com" . browse-url-background-chrome)
        ("https:\\/\\/ui\\.honeycomb\\.io" . browse-url-background-chrome)
        ("https:\\/\\/www\\.figma\\.com" . browse-url-background-chrome)
        ;; Default browser for everything else.
        ("." . browse-url-background)))

(defun browse-url-mpv (url &optional single)
  (let ((process-connection-type nil))
        (start-process "mpv" "mpv" "mpv" url)))

(defun browse-url-background (url &optional single)
  (if (string= system-type "darwin")
      (start-process "open" nil "open" "-g" url)
    (start-process "qutebrowser" nil "qutebrowser" url)))

(defun browse-url-background-chrome (url &optional single)
  (if (string= system-type "darwin")
      (start-process "open" nil "open" "-a" "Google Chrome" "-g" url)
    (start-process "google-chrome-stable" nil "google-chrome-stable" url)))

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

(use-package graphql-mode
  :custom
  ;; sourcegraph style
  (graphql-indent-level 4))

(use-package bongo
  :custom
  (bongo-enabled-backends '(mpv))
  (bongo-custom-backend-matchers '((mpv (local-file) "opus")))
  (bongo-default-directory "~/youtube/feed/music")
  (bongo-mpv-extra-arguments '("--no-video" "--no-audio-display"))
  :config
  (add-to-list 'bongo-audio-file-name-extensions "opus")
  :commands (bongo))

(defun k/kill-word-or-region ()
  "kills region if region is active otherwise kills word"
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-region)
    (call-interactively #'backward-kill-word)))

(global-set-key (kbd "C-w") 'k/kill-word-or-region)

(use-package expand-region
  :bind (("C-+" . er/contract-region)
         ("C-=" . er/expand-region)))

(use-package mosey
  :bind (("C-a" . mosey-backward-bounce)
         ("C-e" . mosey-forward-bounce)))

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package eat
  :demand t
  ;; The build recipe in the package registary doesn't include terminfo and
  ;; integration dirs which breaks shell integration.
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :bind (("C-c h" . eat-project))
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (defun my-eat-shell ()
    (interactive)
    (eat "bash"))
  (bind-key "C-c j" #'my-eat-shell)

  (defun my-eat-ssh (&optional arg)
    (interactive)
    (let* ((host (if (string= (system-name) "habitat") "fa.local" "habitat"))
           (eat-buffer-name (concat "*ssh-" host "-eat*")))
      (eat (concat "ssh " host) arg))))

(use-package detached
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)
           (detached-notification-function #'detached-state-transitionion-echo-message)))

(use-package mastodon
  :custom ((mastodon-instance-url "https://emacs.ch")
           (mastodon-active-user "keegan")))

(use-package chatgpt-shell
  :ensure t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pick-first-password :host "api.openai.com")))))

(use-package protobuf-mode)

(use-package bazel
  :mode ("\\.bazelrc\\'" . bazelrc-mode)) ; sourcegraph uses .bazelrc as an extension

(use-package presentation)

(use-package yasnippet
  :init (yas-global-mode t))

(use-package yasnippet-snippets)

(use-package revbufs)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun load-file-exists (file)
  "Load the Lisp file named FILE if it exists."
  (if (file-exists-p file)
    (load-file file)))

(setq custom-file "~/.emacs.d/custom.el")
(load-file-exists custom-file)

;;; init.el ends here
