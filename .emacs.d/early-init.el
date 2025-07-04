;; perf: use more memory to reduce GC rate. Unbounded at startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024)))) ;; 50mb

(setq frame-inhibit-implied-resize t)

;; https://github.com/d12frosted/homebrew-emacs-plus#no-titlebar
(setq frame-resize-pixelwise t)

;; Disable GUI elements
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq
 use-dialog-box t
 use-file-dialog nil)

(setq
 inhibit-startup-screen t
 inhibit-startup-buffer-menu t)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                native-comp-speed                        2
                package-native-compile                   t))  ;; Compile installed packages
