;; Code from http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html

(defvar required-packages
  '(
    ace-jump-mode
    apache-mode
    coffee-mode
    flycheck
    nginx-mode
    php-mode
    puppet-mode
    smex
    ) "a list of packages to ensure are installed at launch.")

(require 'cl)

;; Setup ELPA
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  (dolist (p required-packages)
    (when (not (package-installed-p p))
            (package-install p))))
