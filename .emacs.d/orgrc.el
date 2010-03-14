(setq org-replace-disputed-keys t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)

(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(global-set-key (kbd "C-M-r") 'remember)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq
 org-agenda-restore-windows-after-quit t
 org-replace-disputed-keys t
 org-agenda-files '("~/org-files/todo.org" "~/org-files/birthdays.org")
 org-agenda-ndays 7
 org-deadline-warning-days 14
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday nil
 org-reverse-note-order t
 org-fast-tag-selection-single-key 'expert
 org-remember-store-without-prompt t
 org-remember-templates (quote
                         ((97  "* APPT %?\n  %u" "~/org-files/todo.org" "Tasks")
                          (116 "* TODO %?\n  %u" "~/org-files/todo.org" "Tasks")))
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