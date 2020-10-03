
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))


(require 'package)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
	(magit crontab-mode eglot yaml-mode toml-mode company-lsp lsp-ui lsp-mode rustic rust-auto-use flycheck-demjsonlint bazel-mode protobuf-mode company-jedi company-emacs-eclim eclim flycheck-rust rust-mode rjsx-mode flow-minor-mode web-mode multiple-cursors flycheck-nim nim-mode flymake-jslint flymake-jshint smart-tabs-mode cmake-mode sage-shell-mode flymd company-irony company sml-mode tide irony bison-mode typescript forth-mode julia-mode markdown-mode racket-mode ## opencl-mode auctex haskell-mode lua-mode js2-mode))))
(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))



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
  :hook (prog-mode . company-mode)
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

; Flycheck
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

; lsp
(use-package lsp-mode
  :commands lsp)
;;:config (require 'lsp-clients))

(use-package lsp-ui)
;; Allow company to get stuff from lsp
;(use-package company-lsp)

;; lsp keybinds
(with-eval-after-load "lsp-mode"
  ;;(bind-key "C-c C-c" 'compile tide-mode-map)
  (bind-key "C-c C-f" 'lsp-execute-code-action lsp-mode-map)
  )


;; Rust stuff
(use-package toml-mode)

;; (use-package rustic-mode
;;   :hook (rustic-mode . lsp)
;;   :config
;;   (bind-key "C-c C-r" 'rustic-run rustic-mode-map)
;;   (bind-key "C-c C-f" 'lsp-execute-code-action rustic-mode-map)
;;   )

(use-package rustic
  :commands (cargo-minor-mode)
  :mode ("\\.rs" . rustic-mode)
  :config
  (bind-keys :map rustic-mode-map
             ("C-c TAB" . rustic-format-buffer)
             ("C-c C-f" . lsp-execute-code-action)
			 ("C-c C-c" . rustic-cargo-build)
			 ("C-c f" . rustic-format-buffer))
  :init
  (setq company-tooltip-align-annotations t
        rustic-format-on-save nil)
  (add-hook 'rustic-mode-hook #'cargo-minor-mode)
  (add-hook 'rustic-mode-hook #'flycheck-mode)
  (add-hook 'rustic-mode-hook #'lsp)
;  (add-hook 'rustic-mode-hook #'lsp-ui)
  )

;; (with-eval-after-load "rustic-mode"
;;   ;;(bind-key "C-c C-c" 'compile tide-mode-map)
;;   )

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rustic-mode . cargo-minor-mode))

;;(use-package flycheck-rust
;;  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))




(setq-default tab-width 4) ; or any other preferred value
(setq cua-auto-tabify-rectangles nil)

(smart-tabs-insinuate 'c++ 'javascript 'java 'c)
(smart-tabs-advice js2-indent-line js2-basic-offset)

(global-set-key (kbd "C-c C-r") 'mc/mark-all-like-this) ; Ctrl+c r
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
;;(require 'flymake-jslint)
;;(add-hook 'js-mode-hook 'flymake-jslint-load)

;; Window resizing
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)


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
;;  :bind ("C-c C-c" . compile)
;;  :bind ("C-c C-f" . tide-fix)
  )
(with-eval-after-load "tide"
  (bind-key "C-c C-c" 'compile tide-mode-map)
  (bind-key "C-c C-f" 'tide-fix tide-mode-map)
  )




;; End Typescript
(put 'downcase-region 'disabled nil)


;; Flymd browser
;; (defun my-flymd-browser-function (url)
;;   (let ((process-environment (browse-url-process-environment)))
;;     (apply 'start-process
;;            (concat "firefox " url)
;;            nil
;;            "/usr/bin/open"
;;            (list "-a" "firefox" url))))
;; (setq flymd-browser-open-function 'my-flymd-browser-function)

(defun my-flymd-browser-function (url)
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; Company Jedi for python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(setq python-shell-interpreter "python3")

(defun cm:jedi-setup-server ()
  "Setup Jedi server, update and reload."
  (interactive)
  (if (not (equal python-environment-default-root-name (cm:jedi-environment-root-name)))
      (progn
        (custom-set-variables
         '(python-environment-default-root-name (cm:jedi-environment-root-name))
         '(jedi:server-command
           (concat (venv-name-to-dir (cm:jedi-environment-root-name)) "bin/jediepcserver")))
        (jedi:install-server-block))))



;; Company Eclim for java

;; (setq eclimd-autostart t)
;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("/Applications/Eclipse.app/Contents/Eclipse"))
;;  '(eclim-executable "/Users/matthew/.p2/pool/plugins/org.eclim_2.8.0/bin/eclim")
;;  )
;; (company-emacs-eclim-setup)
;; (add-hook 'java-mode-hook 'eclim-mode)
;; (with-eval-after-load "eclim"
;;   ;;  (define-key eclim-mode-map "C-c C-c" 'eclim-project-build)
;;   (bind-key "C-c C-c" 'eclim-project-build eclim-mode-map)
;;   (bind-key "C-c C-f" 'eclim-problems-correct eclim-mode-map)
;;   )

;; Bazel BUILD file formatting
(add-hook 'bazel-mode-hook (lambda () (add-hook 'before-save-hook #'bazel-format nil t)))

;; (require 'eclimd)
