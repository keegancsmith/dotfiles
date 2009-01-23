;; c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Django Templates
(autoload 'django-html-mode "django-html-mode" "Django HTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.djhtml$'" . django-html-mode))


;; reST Mode
(autoload 'rst-mode "rst" "mode for editing reStructuredText documents" t)
(add-to-list 'auto-mode-alist '("\\.re?st\\'" . rst-mode))
(add-hook 'rst-mode-hook
          (lambda ()
            (flyspell-mode t)
            (auto-fill-mode t)))


;; Apache
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
(add-to-list 'auto-mode-alist '("apache[12]\?\\.conf$" . apache-mode))
(add-to-list 'auto-mode-alist '("commonapache[12]\?\\.conf$" . apache-mode))


;; LaTeX - Turn on spellcheck and fill mode
(add-hook 'latex-mode-hook
          (lambda ()
            (flyspell-mode t)
            (auto-fill-mode t)))


;; Haskell
(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file.el")


;; Python
(setq pymacs-load-path '("~/.emacs.d/python/"))
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-shortcuts t
      ropemacs-guess-project t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key ropemacs-local-keymap "\M-/" 'dabbrev-expand)))


;; Python ac-source
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))
(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

;; Auto-Complete
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (set-face-background 'ac-selection-face "steelblue")
  (set-face-background 'ac-menu-face "skyblue")
  (define-key ac-complete-mode-map "\r" 'ac-complete)
  (global-set-key "\M-." 'ac-start)
  (setq ac-auto-start nil)
  (setq ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
 
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources '(ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  (add-hook 'python-mode-hook
            (lambda ()
              (make-local-variable 'ac-sources)
              (setq ac-sources '(ac-source-rope)))))
