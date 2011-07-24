;; Function is used in txt based modes such as rst or latex
(defun my-txt-mode-hook ()
  (flyspell-mode t)
  (auto-fill-mode t))


;; c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-variable 'c-basic-offset 4)
(c-set-offset 'access-label -2)
(c-set-offset 'case-label '+)


;; EDiff customizations. Use a tiling window manager so much prefer ediff to
;; not float it's control window
(setq
 ediff-window-setup-function 'ediff-setup-windows-plain
 ediff-split-window-function 'split-window-horizontally)

;; Django Templates
(autoload 'django-html-mode "django-html-mode" "Django HTML Mode" t)
(add-to-list 'auto-mode-alist '("\\.djhtml$'" . django-html-mode))


;; reST Mode
(autoload 'rst-mode "rst" "mode for editing reStructuredText documents" t)
(add-to-list 'auto-mode-alist '("\\.re?st\\'" . rst-mode))
(add-hook 'rst-mode-hook 'my-txt-mode-hook)
(setq rst-adornment-faces-alist
      '((t . font-lock-keyword-face)
        (nil . font-lock-keyword-face)))


;; Apache
(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
(add-to-list 'auto-mode-alist '("apache[12]\?\\.conf$" . apache-mode))
(add-to-list 'auto-mode-alist '("commonapache[12]\?\\.conf$" . apache-mode))


;; PHP
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))


;; LaTeX - Turn on spellcheck and fill mode
(add-hook 'latex-mode-hook 'my-txt-mode-hook)


;; Haskell
;(load "~/.emacs.d/elisp/haskell-mode/haskell-site-file.el")
(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))
