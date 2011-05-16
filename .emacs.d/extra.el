;; Color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-twilight)))


;; Better background for Flymake. For some reason it needs to go after
;; global-font-lock-mode
(when (load "flymake" t)
  (set-face-background 'flymake-errline  "DarkRed")
  (set-face-background 'flymake-warnline "DarkBlue"))


;; Flymake does not recognize warnings in GCC 4.5, fix this
(add-to-list
 'flymake-err-line-patterns
 '(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(	\n]+\\):\\([0-9]+\\):[0-9]+:[ 	\n]*\\(.+\\)" 2 4 nil 5))

;; Dired Stuff
(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq (or ".pyc" ".o") eol)    ;; build artifacts
              (seq bol "." (not (any "."))) ;; hidden files
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)


;; Some useful functions
(defun nxml-pretty-print-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))


; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02 
(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
