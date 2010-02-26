;; emacs general config. Should work with a bare install of emacs


;; Compile lisp files in elisp dir then add elisp to path
(byte-recompile-directory "~/.emacs.d" 0)
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Call extra elisp files
(mapc (lambda (file)
        (if (file-exists-p (concat file ".el"))
            (load file)))
      (mapcar (lambda (file) ;; Get filename with directory
                (expand-file-name file "~/.emacs.d"))
              '("pwords"      ;; Passwords
                "ercrc"       ;; IRC client
                "extra"       ;; Extra packages
                "modes"       ;; Adding editing modes
                "orgrc"       ;; Org-mode
                "qt"          ;; Syntax highlighting for Qt
                "general")))  ;; My old .emacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "<[^>
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/org-files/todo.org" "~/org-files/birthdays.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/org-files/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((97 "* APPT %?
  %u" "~/org-files/todo.org" "Tasks") (116 "* TODO %?
  %u" "~/org-files/todo.org" "Tasks"))))
 '(org-replace-disputed-keys t t)
 '(org-reverse-note-order t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(rst-adornment-faces-alist (quote ((t . font-lock-keyword-face) (nil . font-lock-keyword-face)))))
