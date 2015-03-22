;; emacs general config. Should work with a bare install of emacs

(defun setup-window-system-frame-colours (&rest frame)
  (if window-system
      (let ((f (if (car frame)
                   (car frame)
                 (selected-frame))))
        (progn
          (require 'color-theme)
          (set-frame-font "Inconsolata-13")))))



;; Email
(setq user-full-name "Keegan Carruthers-Smith")
(setq user-mail-address "keegan.csmith@gmail.com")


;; Highlight matching brackets
(show-paren-mode 1)


;; Get rid of stupid GUI stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Yes-or-No queries become Y-or-N
(fset 'yes-or-no-p 'y-or-n-p)


;; Full Screen Mode
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth))
  (display-time-mode (if (frame-parameter nil 'fullscreen) 1 0)))


;; Shift arrow key to move between windows/panes
(windmove-default-keybindings)


;; IDO
(ido-mode t)
(setq
 ido-ignore-buffers
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/fbcode" "~/www" "~/repos" "~/org-files" "~/")
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-max-work-directory-list 30
 ido-max-work-file-list 50
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-enable-flex-matching t
 ido-max-prospects 8
 ido-confirm-unique-completion t)


;; Better buffer names when names conflict
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix nil)

;; Misc settings
(setq
 vc-handled-backends ()
 inhibit-startup-message t
 x-select-enable-clipboard t
 make-backup-files nil
 column-number-mode t
 case-fold-search t
 current-language-environment "English"
 confirm-nonexistent-file-or-buffer nil
 compilation-window-height 10
 compilation-scroll-output t
 dabbrev-case-fold-search t
 save-abbrevs nil
 font-lock-maximum-decoration t
 vc-follow-symlinks t
 vc-bzr-diff-switches t ;; Make bzr diffs normal
 tramp-default-method "ssh"
 tramp-auto-save-directory "~/.emacs.d/tramp-autosave")
(global-font-lock-mode 1)


;; Misc buffer settings
(setq-default
 fill-column 78
 indent-tabs-mode nil)


;; Shortcuts
(global-set-key (kbd "<f5>")     'previous-error)
(global-set-key (kbd "<f6>")     'next-error)
(global-set-key (kbd "<f7>")     'flymake-mode)
(global-set-key (kbd "<f9>")     'compile)
(global-set-key (kbd "<f11>")    'fullscreen)
(global-set-key (kbd "C-g")      'goto-line)
(global-set-key (kbd "C-d")      'comment-region)
(global-set-key (kbd "C-S-d")    'uncomment-region)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-c c")    'comment-dwim)


;; Shortcuts for easier window resizing
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)


;; Disable annoying keys I accidently hit
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))
(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Enable Full Screen mode
;;(fullscreen)
