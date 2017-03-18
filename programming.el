;; Set default tab settings
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Set the tab width
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode 1)
  :config
  (use-package smartparens-config))


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

;; YASnippet
;(require 'yasnippet)
;(yas-global-mode 1)

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
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config 
  (js2r-add-keybindings-with-prefix "C-c C-m")
  ;; eg. extract function with `C-c C-m ef`.
  )

(use-package tern
  :ensure t
  :init
  (add-hook 'js-mode-hook  (lambda () (tern-mode t))))

(use-package company-tern
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package json-mode
  :ensure t
  :mode "\\.json'")

(use-package yaml-mode
  :ensure t
  :mode "\\.yml'")

;;;;;;;;; F#

;; TODO Setup autocomplete for F# properly
(use-package fsharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
  (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/local/bin/fsharpc"))


;; temporary hack for cases when not modifying whitespace is important
;; (defun delete-trailing-whitespace ()
;;   (interactive))
;; (setq standard-indent 8)
;; (setq-default indent-tabs-mode 1)
;; (defun untabify (start end)
;;   (interactive))

;; org-babel

(use-package ob-fsharp
  :load-path "site-lisp/")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (fsharp . t)
   (js . t)))

(setq org-babel-python-command "python3")

(provide 'programming)
