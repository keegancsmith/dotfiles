;; ERC config

;; Load private data
(if (file-exists-p "~/.emacs.d/ercprivate.el")
    (load "~/.emacs.d/ercprivate.el"))

;; Main Settings
(setq erc-nick "casted"
      erc-user-full-name "Keegan"
      erc-email-userid "keegan@haha.com"
      erc-auto-query 'bury
      erc-join-buffer 'bury
      erc-hide-list '("JOIN" "PART" "QUIT")
      erc-log-channels-directory nil)

;; Extra Mode settings
(require 'erc-match)
(setq erc-keywords '("casted" "keegan"))
(erc-match-mode)

(erc-scrolltobottom-mode t)

;; Don't track boring activity
(require 'erc-track)
(erc-track-mode t)
(setq erc-track-exclude '("*highlight") ;; ZNC highlight buffer
      erc-track-exclude-server-buffer t
      erc-track-exclude-types
      '("JOIN" "NICK" "PART" "QUIT" "MODE"
        "324" "329" "332" "333" "353" "477"))

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
  (erc-cmd-ME
   (concat " is listening to "
           (substring
            (shell-command-to-string "mpc -f '%artist% - %title%' | head -n1")
            0 -1))))


(defun erc-cmd-CLEAR ()
  "Clears the current buffer"
  (erc-truncate-buffer-to-size 0))


(defun erc-cmd-CLEARALL ()
  "Clears all ERC buffers"
  (mapc (lambda (buffer)
          (erc-truncate-buffer-to-size 0 (get-buffer buffer)))
        (erc-all-buffer-names)))
