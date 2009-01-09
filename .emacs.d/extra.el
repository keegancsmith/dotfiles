;; Color-theme
(require 'color-theme)
(color-theme-initialize)
(if window-system (color-theme-dark-laptop) (color-theme-euphoria))


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(italic ((t (:slant oblique)))))


;; Better background for Flymake. For some reason it needs to go after
;; global-font-lock-mode
(when (load "flymake" t)
  (set-face-background 'flymake-errline  "DarkRed")
  (set-face-background 'flymake-warnline "DarkBlue"))


;; Dired Stuff
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq bol ".svn" eol)          ;; svn dirs
              (seq ".pyc" eol)
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
