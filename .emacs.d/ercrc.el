;; ERC config

;; Main Settings
(setq erc-server "za.shadowfire.org"
      erc-port 6667 
      erc-nick "casted"
      erc-user-full-name "Keegan"
      erc-email-userid "keegan@haha.com"
      erc-prompt-for-password nil
      erc-auto-query 'bury
      erc-log-channels-directory nil)


;; Notices
(add-hook 'erc-echo-notice-hook 'erc-echo-notice-in-active-non-server-buffer)

;; Identify on connect
(add-hook 'erc-after-connect
	  '(lambda (SERVER NICK)
	     (erc-message "PRIVMSG" (concat
				     "NickServ identify " erc-pword))))


;; Extra Mode settings
(require 'erc-match)
(setq erc-keywords '("casted" "keegan"))
(erc-match-mode)

(erc-scrolltobottom-mode t)

(require 'erc-track)
(erc-track-mode t)
(setq erc-track-exclude-types 
      '("JOIN" "NICK" "PART" "QUIT" "MODE"
        "324" "329" "332" "333" "353" "477"))

(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(require 'erc-spelling)
(erc-spelling-mode 1)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R]")

;; Kill buffers when disconnected or parted
(setq erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t)

;; Truncate buffers to stop some sluggishness
(setq erc-max-buffer-size 20000)
(erc-truncate-mode t)


;; Custom commands

(defun erc-cmd-MEDIA ()
  "Announce the current amarok song to the current ERC channel."
  (let ((dcop '(lambda (val)
		 (substring 
		  (shell-command-to-string
		   (concat "dcop amarok player "
			   val)) 0 -1))))
    (erc-cmd-ME
     (concat " is listening to "
	     (funcall dcop "title")
	     " by "
	     (funcall dcop "artist")
	     " on "
	     (funcall dcop "album")
	     " [Amarok]"))))

(defun erc-cmd-CLEAR ()
  "Clears the current buffer"
  (erc-truncate-buffer-to-size 0))


(defun erc-cmd-CLEARALL ()
  "Clears all ERC buffers"
  (mapc (lambda (buffer)
          (erc-truncate-buffer-to-size 0 (get-buffer buffer)))
        (erc-all-buffer-names)))


(defun erc-cmd-SYSINFO ()
  "System Info!"
  (interactive)
  (erc-cmd-ME
   (concat " "
	   (shell-command-to-string 
	    "perl /home/keegan/bin/sysinfo.pl"))))

(defun erc-cmd-IDENTIFY ()
  "Identify to nickserv"
  (erc-message "PRIVMSG" (concat
			  "NickServ identify " erc-pword)))

(defun erc-cmd-GHOST ()
  "Ghost casted"
  (erc-message "PRIVMSG" (concat
			  "NickServ ghost casted " erc-pword)))

(defun erc-bored ()
  "Connect to a few networks."
  (interactive)
  (erc-select :server "za.shadowfire.org")
  (erc-select :server "reaper.atrum.org"))



;; Nick Colours
;; This code is fail. fix it
;; (setq nick-face-list '())

;; ;; Define the list of colors to use when coloring IRC nicks.                                                                     
;; (setq-default erc-colors-list '("blue" "green" "yellow"
;;                                 "gray" "brown" "red"
;;                                 "purple" "white" "cyan"))

;; (defun build-nick-face-list ()
;;   (setq i -1)
;;   (setq nick-face-list
;;         (mapcar
;;          (lambda (COLOR)
;;            (setq i (1+ i))
;;            (list (custom-declare-face
;;                   (make-symbol (format "erc-nick-face-%d" i))
;;                   (list (list t (list :foreground COLOR)))
;;                   (format "Nick face %d" i))))
;;          erc-colors-list)))

;; (defun erc-color-insert-modify-hook ()
;;   (if (null nick-face-list) (build-nick-face-list))
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (looking-at "<\\([^>]*\\)>")
;;         (let ((nick (match-string 1)))
;;           (put-text-property (match-beginning 1) (match-end 1)
;;                              'face (nth
;;                                     (mod (string-to-number
;;                                           (substring (md5 nick) 0 4) 16)
;;                                          (length nick-face-list))
;;                                     nick-face-list))))))

;; ;; This adds the ERC message insert hook.                                                                                        
;; (add-hook 'erc-insert-modify-hook 'erc-color-insert-modify-hook)
