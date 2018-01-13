
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key key-translation-map [?\C-h] [?\C-?])

(setq-default transient-mark-mode t)
(setq make-backup-files nil)

;; theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#111111")))
;; (load-theme 'zenburn t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(current-language-environment "Japanese")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("1373e3623ed5d758ef06dd19f2c8a736a69a15496c745a113d42230ab71d6b58" "799291799f87afb7a2a55bd63082c58fb58912bee0a6e3d5c1ce0e083ed046c9" default)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 98 :width normal)))))
