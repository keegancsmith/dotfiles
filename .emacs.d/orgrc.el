(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(global-set-key (kbd "C-M-r") 'remember)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-directory "~/Dropbox/org-files")
(defun org-file (fname)
  (concat org-directory "/" fname ".org"))

(setq
 org-agenda-restore-windows-after-quit t
 org-replace-disputed-keys t
 org-agenda-files (mapcar 'org-file '("todo" "birthdays"))
 org-agenda-ndays 7
 org-deadline-warning-days 14
 org-default-notes-file (org-file "todo")
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday nil
 org-reverse-note-order t
 org-fast-tag-selection-single-key 'expert
 org-remember-store-without-prompt t
 org-remember-templates (quote
                         ((97  "* APPT %?\n  %u"     (org-file "todo") "Tasks")
                          (84  "* TODO %?\n  %u\n%a" (org-file "todo") "Tasks")
                          (116 "* TODO %?\n  %u"     (org-file "todo") "Tasks")))
 remember-annotation-functions '(org-remember-annotation)
 remember-handler-functions '(org-remember-handler)
 org-agenda-custom-commands
 (quote (("c" todo "DONE|DEFERRED|CANCELLED" nil)
         ("u" alltodo ""
          ((org-agenda-skip-function
            (lambda nil
              (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                        (quote regexp) "<[^>\n]+>")))
           (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

(eval-after-load "org"
  '(progn
     (set-face-foreground 'org-level-2 "orange")))
