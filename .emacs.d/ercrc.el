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

(add-hook 'erc-mode-hook (lambda () (auto-fill-mode 0)))

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


;; A Bar for delimiting read and unread message
;; http://www.emacswiki.org/emacs/ErcBar
(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
              (count (cadr info)))
         (if (and info (> count erc-bar-threshold))
             (save-excursion
               (end-of-buffer)
               (when (erc-bar-move-back count)
                 (let ((inhibit-field-text-motion t))
                   (move-overlay erc-bar-overlay
                                 (line-beginning-position)
                                 (line-end-position)
                                 (current-buffer)))))
           (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "red"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
                                      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
                                          (erc-bar-update-overlay)))))
