(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package auto-package-update
  :ensure
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package company
  :ensure
  :bind
  (:map company-active-map
        ("TAB". company-complete-common-or-cycle)
        ("C-n". company-select-next)
        ("C-p". company-select-previous))
  (:map company-mode-map
	("TAB". company-indent-or-complete-common))

  :config (global-company-mode)
					;(define-key company-mode-map [remap c-indent-line-or-region] 'company-indent-or-complete-common)
  )

(use-package flycheck :ensure)
(use-package typescript-mode :ensure)

(defun debug-tide-hook () (message "DEBUG: hooked tide"))

;; TypeScript
(use-package tide
  :ensure
  :init (message "DEBUG: tide init")
  ;;:mode "\\.ts\\'"
  ;;:after (typescript-mode company flycheck)
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
        (typescript-mode . setup-tide-mode)
        (typescript-mode . tide-hl-identifier-mode)
        (typescript-mode . company-mode)
        (typescript-mode . debug-tide-hook)
        (before-save . tide-format-before-save))
  :config (defun setup-tide-mode ()
	    (message "DEBUG: Running setup-tide-mode")
	    (tide-setup)
	    (flycheck-mode +1)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode +1)
	    (tide-hl-identifier-mode +1)
	    ;; company is an optional dependency. You have to
	    ;; install it separately via package-install
	    ;; `M-x package-install [ret] company`
	    (company-mode +1)))



(use-package ivy
  :ensure
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package cmake-mode auto-package-update)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
