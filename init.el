(require 'package)

(package-initialize)

(setq column-number-mode t)

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
  :hook (after-init . global-company-mode)
  :config
  (setq company-backends
	'((company-capf)
	  (company-clang)
	  (company-files))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(use-package ivy
  :ensure
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package projectile
  :ensure
  :bind (:map projectile-mode-map
	      ("C-c p" . 'projectile-command-map))
  :config (projectile-mode))

(use-package minimap :ensure)

(use-package flycheck :ensure)

(use-package which-key
  :ensure
  :config (which-key-mode))

(use-package project :ensure)

(use-package multiple-cursors
  :ensure
  :bind (("C-c C-l" . mc/edit-lines)))

(use-package browse-at-remote
  :ensure
  :bind (("C-c g g" . browse-at-remote)))

(use-package lsp-mode
  :ensure
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (c++-mode . lsp)
         (js-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
	      ("C-c C-f" . lsp-execute-code-action)
              ("M-." . lsp-goto-type-definition)
;              ("M->" . lsp-goto-implementation)
              )
  :custom
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]venv\\'")
  (lsp-keymap-prefix "C-c l")
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; This controls the overlays that display type and other hints inline. Enable
  ;; / disable as you prefer. Well require a `lsp-workspace-restart' to have an
  ;; effect on open projects.
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil))
;  :config
  ;(add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;; TypeScript
(use-package typescript-mode :ensure)
(use-package tide
  :ensure
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
        (typescript-mode . setup-tide-mode)
        (typescript-mode . tide-hl-identifier-mode)
        ;;(before-save . tide-format-before-save)
)
  :bind (:map tide-mode-map
              ("C-c C-f" . tide-fix)
              ("C-c C-r" . tide-rename)
              ("C-c C-o" . tide-organize-imports)
              ("C-c C-c" . comment-region)
              ("C-c C-u" . uncomment-region))
  :config (defun setup-tide-mode ()
	    (tide-setup)
            (setq tide-tsserver-process-environment '("NODE_OPTIONS=--max-old-space-size=32768"))
	    (flycheck-mode +1)
	    (setq flycheck-check-syntax-automatically '(save mode-enabled))
	    (eldoc-mode +1)
	    (tide-hl-identifier-mode +1)
	    (company-mode +1)
	    (display-fill-column-indicator-mode)))


(use-package magit :ensure)
(use-package bazel
  :ensure
  :config
  (setq bazel-buildifier-before-save t))

(use-package json-mode
  :ensure
  :hook (json-mode . (lambda ()
	   (make-local-variable 'js-indent-level)
           (setq js-indent-level 2))))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (setq lsp-prefer-capf t)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

(setq-default indent-tabs-mode nil)

(use-package lua-mode :ensure)
(use-package yaml-mode
  :ensure
  :hook (yaml-mode . lsp))

(use-package dockerfile-mode :ensure)

(use-package lsp-java :ensure)

(use-package csharp-mode
  :ensure
  :init
  (defun my/csharp-mode-hook ()
    (lsp))
  (add-hook 'csharp-mode-hook #'my/csharp-mode-hook))

(setenv "FrameworkPathOverride" "/opt/homebrew/Cellar/mono/6.12.0.182")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)
 '(package-selected-packages
   '(csharp-mode yaml-mode which-key use-package typescript-mode toml-mode tide rustic projectile multiple-cursors minimap magit lua-mode lsp-ui lsp-pyright lsp-java json-mode ivy exec-path-from-shell eglot dockerfile-mode company-quickhelp company-irony company-emacs-eclim cmake-mode browse-at-remote bazel auto-package-update))
 '(tide-format-options nil)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
