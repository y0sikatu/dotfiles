;;; init.el --- provide init.el

;;; Commentary:

;;; Code:

;; 
(add-to-list 'load-path (locate-user-emacs-file "conf"))

;; Load proxy settings if exists.
(load "proxy-conf" t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package):
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) ;; melpa
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t) ;;melpa-stable
(package-initialize)
;; install if not installed
(defvar installing-package-list
  '(
	company
	flycheck
	helm
	yasnippet
	zenburn-theme
	))
(require 'cl) ;; for loop
(let ((not-installed (loop for x in installing-package-list
						   when (not (package-installed-p x))
						   collect x)))
  (when not-installed
	(package-refresh-contents)
	(dolist (pkg not-installed)
	  (package-install pkg))))

;; key bind
(define-key minibuffer-local-filename-completion-map " " 'minibuffer-complete-word)
(define-key key-translation-map [?\C-h] [?\C-?])

;; misc
(setq-default transient-mark-mode t)
(setq-default tab-width 4)
(setq make-backup-files nil)
(global-linum-mode)
(column-number-mode t)
(global-hl-line-mode t)
(show-paren-mode t)
(tool-bar-mode 0)

;; c++-mode
(add-hook 'c++-mode-hook
		  '(lambda()
			 (setq-default indent-tabs-mode nil)
			 (c-set-style "stroustrup")
			 (c-set-offset 'innamespace 0)
			 (set (make-local-variable 'company-backends)
				  '((company-dabbrev-code company-yasnippet)))
			 ))

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
(global-set-key (kbd "C-c y") 'company-yasnippet)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; helm
(require 'helm-config)
(helm-mode 1)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; zenburn-theme
(defvar zenburn-override-colors-alist
  '(("zenburn-bg" . "#111111")))
(load-theme 'zenburn t)

;; customize emacs
(setq custom-file (locate-user-emacs-file "conf/custom.el"))
(load custom-file t)

(provide 'init)
;;; init.el ends here
