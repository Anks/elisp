;; Set default tab settings
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
;; Set the tab width
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Smartparens

(setq show-paren-delay 0)
(show-paren-mode)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode 1)
  :config
  (use-package smartparens-config))

;;; Emmet

;; Hide warning for (require 'cl)
;; Emmet requires 'cl, which is deprecated.
;; But it hasn't yet been updated to migrate to 'cl-lib

(setq byte-compile-warnings '(cl-functions))


(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)  ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)  ;; enable Emmet's css abbreviation.
  ;; (setq emmet-expand-jsx-className? t) ;;  TODO enable this only for react projects
  ;; Remind myself to use emmet
  (defun anks-emmet-reminder ()
    (message "emmet-mode enabled. Use C-j!"))

  (add-hook 'sgml-mode-hook 'anks-emmet-reminder)
  (add-hook 'css-mode-hook 'anks-emmet-reminder)

  )

;;; Auto complete using company mode
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode 1))


;; Terraform
(use-package terraform-mode
  :ensure t)

;; YASnippet
;(require 'yasnippet)
;(yas-global-mode 1)

;; projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (use-package ag
    :ensure t)
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;; Language specific modes

;; xml-mode
(autoload 'nxml-mode "nxml-mode" nil t)
(defalias 'xml-mode 'nxml-mode)

;; via https://github.com/pkkm/.emacs.d/blob/master/conf/mode-specific/javascript.el

;; Javascript IDE.
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :config
  (setq js2-highlight-level 3) ; Highlight many built-in functions.
  (setq js2-global-externs '("require" "module" "console")))

(use-package js2-refactor
  :ensure t
  :init
  (defun anks-js2-reminder ()
    (message "js2-refactor prefixed by C-c C-m"))
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (add-hook 'js2-mode-hook 'anks-js2-reminder)
  :config 
  (js2r-add-keybindings-with-prefix "C-c C-m")
  ;; eg. extract function with `C-c C-m ef`.
  )

(use-package tern
  :ensure t
  :init
  (add-hook 'js-mode-hook  (lambda () (tern-mode t))))

(use-package json-mode
  :ensure t
  :mode "\\.json'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml'")


;;;; EditorConfig
(use-package editorconfig
  :diminish editorconfig-mode
  :ensure t
  :config
  (editorconfig-mode 1))

;;;;;; Restclient

(use-package restclient
  :ensure t)

(use-package ob-restclient
  :ensure t)

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

;; org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (js . t)
   (restclient . t))

(setq org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2017.20/libexec/plantuml.jar")
(setq org-babel-python-command "python3")

;; Find other file
(add-hook
 'c-mode-common-hook
 (lambda
   ()
   (local-set-key
    (kbd "C-c o")
    'ff-find-other-file)))


;; eglot mode -- lsp integration
(use-package eglot
  :ensure t)

(provide 'programming)
