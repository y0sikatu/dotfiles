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
	company-irony
	flycheck
	helm
	helm-gtags
	helm-projectile
	irony
	rtags
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
(setq-default inhibit-startup-screen t)
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
			 ))

;; company
(require 'company)
(global-company-mode 1) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
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
(require 'helm)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(helm-autoresize-mode 1)
(helm-mode 1)

;; helm-gtags
(custom-set-variables
 ;;  '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-auto-update t)
 )
(require 'helm-gtags)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "C-c t") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-c r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "C-c s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "C-c p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  )

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

;; irony
(require 'irony)
;; (add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-common-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-to-list 'company-backends 'company-irony)

;; rtags
;; (require 'rtags)
;; (add-hook 'c-mode-common-hook
;; 		  '(lambda()
;; 			(when (rtags-is-indexed)
;; 			  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
;; 			  (local-set-key (kbd "M-;") 'rtags-find-symbol)
;; 			  (local-set-key (kbd "M-@") 'rtags-find-references)
;; 			  (local-set-key (kbd "M-,") 'rtags-location-stack-back))))
;; (custom-set-variables '(rtags-use-helm t))

;; yasnippet
(require 'yasnippet)
;; companyと競合するのでyasnippetのfield移動は "C-i" のみにする
(define-key yas-keymap (kbd "<tab>") nil)
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
