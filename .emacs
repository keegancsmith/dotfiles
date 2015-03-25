;; emacs general config. Should work with a bare install of emacs

(add-to-list 'load-path "~/.emacs.d/elisp")

;; Setup ELPA before anything else
(setq package-enable-at-startup nil)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Call extra elisp files
(mapc (lambda (file)
        (if (file-exists-p (concat file ".el"))
            (load file)))
      (mapcar (lambda (file) ;; Get filename with directory
                (expand-file-name file "~/.emacs.d"))
              '("pwords"      ;; Passwords
                "ercrc"       ;; IRC client
                "extra"       ;; Extra packages
                "modes"       ;; Adding editing modes
                "orgrc"       ;; Org-mode
                "qt"          ;; Syntax highlighting for Qt
                "general")))  ;; My old .emacs
