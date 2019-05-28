;; emacs general config. Should work with a bare install of emacs

;;; Code:

;; Install packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
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
 compilation-scroll-output t
 dabbrev-case-fold-search t
 save-abbrevs nil
 font-lock-maximum-decoration t
 vc-follow-symlinks t
 display-time-world-list
 '(("Africa/Johannesburg" "Cape Town")
   ("America/Los_Angeles" "San Francisco")
   ("Europe/London" "London")
   ("Europe/Berlin" "Berlin")))
;; Misc buffer settings
(setq-default
 fill-column 78
 tab-width 4
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

(global-set-key (kbd "C-c C-c") 'compile)

;; go's present tool slides don't have a major mode for emacs, so ensure they
;; are opened as text files.
(add-to-list 'auto-mode-alist '("\\.slide\\'" . text-mode))

(setq use-package-always-ensure t)

(use-package monokai-theme)

(use-package avy
  :ensure t
  :bind ("C-c SPC" . avy-goto-char))

(use-package go-mode
  :defer t
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my-go-mode-hook ()
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go test")))
  (add-hook 'go-mode-hook #'my-go-mode-hook)

  (use-package go-rename)
  (use-package go-guru
    :config
    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  ;; Make flycheck use the same load path so it doesn't complain about require
  ;; statements in elisp.
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package ivy
  :ensure t
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

  (load-file "~/go/src/github.com/keegancsmith/counsel-repo/counsel-repo.el")
  (setq
   counsel-repo-srcpaths '("~/go/src" "~/src")
   counsel-repo-action #'magit-status)
  (global-set-key (kbd "C-c j") #'counsel-repo)

  ;; Avoid long lines in counsel-rg
  (setq counsel-rg-base-command "rg -i -g '!vendor' --no-heading --line-number --color never --max-columns 200 %s ."))

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c C-x C-j" . org-clock-goto))
  :config
  (use-package org-journal
    :config
    (setq
     org-journal-dir "~/org-files/journal/"
     org-journal-file-format "%Y%m%d.org"))

  (require 'org-protocol)
  ;; https://orgmode.org/worg/org-contrib/org-protocol.html
  ;; https://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
  ;; depends on script:
  ;;   emacsclient -c -F "((name . \"emacs-capture\") (height . 10) (width . 80))" "$@"
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (progn
          (delete-other-windows)
          (x-focus-frame nil))))
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  (add-to-list 'org-modules 'org-habit)
  (setq
   org-agenda-files '("~/org-files" "~/org-files/journal")
   org-refile-targets '((("~/org-files/work.org" "~/org-files/todo.org") :maxlevel . 1))
   org-deadline-warning-days 14
   org-default-notes-file "~/org-files/todo.org"
   org-reverse-note-order t
   org-fast-tag-selection-single-key 'expert
   org-export-backends '(ascii html icalendar md)
   org-agenda-span 'day
   org-capture-templates
   '(("c" "Task" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U")
     ("w" "Task" entry (file "~/org-files/inbox.org")
      "* TODO %?\n  %U")
     ("o" "P0 ops work scheduled and clocked in now" entry (file+headline "~/org-files/work.org" "Ops")
      "* P0 Ops :urgent:ops:\n  %t\n  %u" :clock-in t :clock-keep t :empty-lines 1)
     ("m" "Meeting now" entry (file+headline "~/org-files/work.org" "Meeting")
      "* %? :meeting:\n  %T" :clock-in t :clock-keep t :jump-to-captured t :empty-lines 1)
     ("L" "Link from org-protocol" entry (file+headline "~/org-files/work.org" "Inbox")
      "* TODO foo\n  SCHEDULED: %t\n  Source: %u, %c\n\n  %i"))))

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
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine)

(use-package dockerfile-mode)

(use-package nim-mode)

(use-package guide-key
  :config
  (setq guide-key/guide-key-sequence t)
  (guide-key-mode 1))

;; For visual wrapping at 80 columns when editing markdown.
(use-package visual-fill-column)

(use-package markdown-mode
  :ensure t
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
  :ensure t
  :config
  (setq edit-server-url-major-mode-alist '(("github\\.com" . gfm-mode))
        edit-server-new-frame nil)
  (edit-server-start))

;; Call secret elisp files
(mapc (lambda (file)
        (if (file-exists-p (concat file ".el"))
            (load file)))
      (mapcar (lambda (file) ;; Get filename with directory
                (expand-file-name file "~/.secret"))
              '("org-gcal"    ;; org-gcal contains secrets
                )))

;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" default)))
 '(package-selected-packages
   (quote
    (visual-fill-column git-timemachine nginx-mode monokai-theme material-theme zenburn-theme exec-path-from-shell nim-mode edit-server use-package go-guru go-rename org-protocol guide-key one-key org-gcal yaml-mode toml-mode paredit org-journal markdown-mode ivy-hydra graphql-mode go-mode flycheck dockerfile-mode counsel blacken))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
