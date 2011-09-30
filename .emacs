;; emacs general config. Should work with a bare install of emacs


;; Compile lisp files in elisp dir then add elisp to path
(add-to-list 'load-path "~/.emacs.d/elisp")
(byte-recompile-directory "~/.emacs.d" 0)

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
