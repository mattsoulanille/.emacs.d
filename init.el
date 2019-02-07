
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))


(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(company-emacs-eclim eclim flycheck-rust rust-mode rjsx-mode flow-minor-mode web-mode multiple-cursors flycheck-nim nim-mode flymake-jslint flymake-jshint smart-tabs-mode cmake-mode sage-shell-mode flymd company-irony company sml-mode tide irony bison-mode typescript forth-mode julia-mode markdown-mode racket-mode ## opencl-mode auctex haskell-mode lua-mode js2-mode))))
(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))




;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	company-backends                '((company-irony company-gtags))
	)
  :bind ("C-c ;" . company-complete-common)
  )


(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
  (define-key company-mode-map [remap c-indent-line-or-region] 'company-indent-or-complete-common)
  ()
  )


(setq-default tab-width 4) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)

(smart-tabs-insinuate 'c++ 'javascript 'java 'c)
(smart-tabs-advice js2-indent-line js2-basic-offset)

(global-set-key (kbd "C-c C-r") 'mc/mark-all-like-this) ; Ctrl+c r
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
;;(require 'flymake-jslint)
;;(add-hook 'js-mode-hook 'flymake-jslint-load)


; TeX
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.


; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind ("C-c C-c" . compile)
  :bind ("C-c C-f" . tide-fix)
  )
;; (with-eval-after-load "tide"
;;   (define-key tide-mode-map "C-c C-c" 'compile)
;;   (define-key tide-mode-map "C-c C-f" 'tide-fix)
;;   )




; End Typescript
(put 'downcase-region 'disabled nil)


; Flymd browser
(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

; Company Eclim for java
(custom-set-variables
 '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
 '(eclim-executable "/Users/matthew/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
 )
(company-emacs-eclim-setup)
(add-hook 'java-mode-hook 'eclim-mode)



;(require 'eclimd)
