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
  :config (global-company-mode))

(use-package ivy
  :ensure
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package flycheck :ensure)

;; TypeScript
(use-package typescript-mode :ensure)
(use-package tide
  :ensure
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
        (typescript-mode . setup-tide-mode)
        (typescript-mode . tide-hl-identifier-mode)
        (typescript-mode . company-mode)
        (before-save . tide-format-before-save))
  :config (defun setup-tide-mode ()
	    (tide-setup)
	    (flycheck-mode +1)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode +1)
	    (tide-hl-identifier-mode +1)
	    (company-mode +1)))


(use-package magit :ensure)
(use-package bazel :ensure)
(use-package json-mode
  :ensure
  :hook (json-mode . (lambda ()
	   (make-local-variable 'js-indent-level)
           (setq js-indent-level 2))))


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
