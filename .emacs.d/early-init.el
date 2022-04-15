;; perf: use more memory to reduce GC rate
(setq gc-cons-threshold 100000000)

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

(setq package-enable-at-startup nil)
