;;; init.el --- provide init.el

;;; Commentary:

;;; Code:

;; proxy
(load "proxy-conf" t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package):
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) ;; melpa
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t) ;;melpa-stable
(package-initialize)
(package-refresh-contents)
(defvar my/favorite-packages
  '(
    zenburn-theme
    company
    flycheck
    ))
;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; key bind
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key key-translation-map [?\C-h] [?\C-?])

;; misc
(setq-default transient-mark-mode t)
(setq make-backup-files nil)
(global-linum-mode)
(column-number-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(setq-default tab-width 4)

;; c++-mode
(add-hook 'c++-mode
	  (lambda()
	    (c-set-style 'stroustrup)))

;; zenburn-theme
(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#111111")))
(load-theme 'zenburn t)

;; company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)
(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))
(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))
(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous) ; おまけ

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(current-language-environment "Japanese")
 '(custom-safe-themes
   (quote
    ("bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "21417b73cddd887276d91053082746f5fab3d8c933a705999595fce1ab1e4ded" "1373e3623ed5d758ef06dd19f2c8a736a69a15496c745a113d42230ab71d6b58" "799291799f87afb7a2a55bd63082c58fb58912bee0a6e3d5c1ce0e083ed046c9" default)))
 '(package-selected-packages (quote (company zenburn-theme)))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 98 :width normal)))))

(provide 'init)
;;; init.el ends here
