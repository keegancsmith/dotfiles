(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key mode-specific-map [?a] 'org-agenda)

(define-key global-map "\C-cc" 'org-capture)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-directory "~/org-files")
(defun org-file (fname)
  (concat org-directory "/" fname ".org"))

(setq
 org-agenda-restore-windows-after-quit t
 org-replace-disputed-keys t
 org-agenda-files (mapcar 'org-file '("todo"))
 org-agenda-ndays 7
 org-deadline-warning-days 14
 org-default-notes-file (org-file "todo")
 org-agenda-show-all-dates t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday nil
 org-reverse-note-order t
 org-fast-tag-selection-single-key 'expert
 org-capture-templates
 '(
	("t" "Task" entry (file+headline "" "Tasks")
		"* TODO %?\n  %u\n  %a")
	("j" "Journal Entry"
	 entry (file+datetree (org-file "journal")
			      "* %?\n  %u"
			      :empty-lines 1)))
 org-agenda-custom-commands
 (quote (("c" todo "DONE|DEFERRED|CANCELLED"
          ((org-agenda-sorting-strategy '(tsia-down))))
	 ("u" alltodo ""
	  ((org-agenda-skip-function
	    (lambda nil
	      (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					(quote regexp) "<[^>\n]+>")))
	   (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

(setq org-todo-keyword-faces
      '(("STARTED" . (:foreground "yellow"))
	("WAITING" . (:foreground "purple"))
	("INREVIEW" . (:foreground "purple"))
	("APPT" . org-todo)))

(eval-after-load "org"
  '(progn
     (set-face-foreground 'org-level-2 "orange")))
