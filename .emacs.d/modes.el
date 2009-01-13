;; c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Django Templates
(autoload 'django-html-mode "django-html-mode" "Django HTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.djhtml$'" . django-html-mode))


;; reST Mode
(autoload 'rst-mode "rst" "mode for editing reStructuredText documents" t)
(add-to-list 'auto-mode-alist '("\\.re?st\\'" . rst-mode))
(setq rst-mode-lazy nil)
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
             (define-key ropemacs-local-keymap "\M-/" 'dabbrev-expand)
             (define-key ropemacs-local-keymap "\M-." 'rope-code-assist)))
