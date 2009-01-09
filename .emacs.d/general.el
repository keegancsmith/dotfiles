;; emacs general config. Should work with a bare install of emacs

;; Xft support if using 23
(if (>= emacs-major-version 23)
    (set-frame-font "Bitstream Vera Sans Mono-9"))
    ;;(set-default-font "Andale Mono-11"))

;; Email
(setq user-full-name "Keegan Carruthers-Smith")
(setq user-mail-address "keegan.csmith@gmail.com")


;; Highlight matching brackets
(show-paren-mode 1)


;; Get rid of stupid GUI stuff
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Indentation
(set-variable 'c-basic-offset 4)
(c-set-offset 'access-label -2)
(c-set-offset 'case-label '+)
(add-hook 'html-mode-hook
          (lambda ()
            (setq indent-line-function 'indent-relative)))


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
(setq ido-enable-flex-matching t)


;; Misc settings
(setq
 inhibit-startup-message t
 x-select-enable-clipboard t
 make-backup-files nil
 column-number-mode t
 case-fold-search t
 current-language-environment "English"
 compilation-window-height 18
 compilation-scroll-output t
 save-abbrevs nil
 font-lock-maximum-decoration t
 tramp-default-method "ssh"
 tramp-auto-save-directory "~/emacs/tramp-autosave")
(global-font-lock-mode 1)

;; Misc buffer settings
(setq-default
 fill-column 78
 indent-tabs-mode nil)


;; Shortcuts
(global-set-key (kbd "<f5>")     'previous-error)
(global-set-key (kbd "<f6>")     'next-error)
(global-set-key (kbd "<f7>")     'flymake-mode)
(global-set-key (kbd "<f8>")     'add-change-log-entry-other-window)
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


;; Enable Full Screen mode
(fullscreen)
