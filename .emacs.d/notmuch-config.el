;;; notmuch-config.el --- notmuch specific configuration

;;; Commentary:

;;; Code:

(require 'ol-notmuch)

(setq
 notmuch-search-oldest-first nil
 ; on gmail sync we will get the file, we don't need to create the sent mail
 notmuch-fcc-dirs nil)

(defun my-adjust-joe-name (args)
  "Replace Joe's unicode GitHub name in ARGS with normal width chars."
  (mapcar (apply-partially #'replace-regexp-in-string "ᴜɴᴋɴᴡᴏɴ" "Joe") args))

(advice-add 'notmuch-search-insert-authors :filter-args  #'my-adjust-joe-name)

;;; notmuch-config.el ends here
