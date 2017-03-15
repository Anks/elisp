;; Set default tab settings
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Set the tab width
(setq tab-width 4)
(setq c-basic-indent 4)

;;; Smartparens
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1)
  :config
  (use-package smartparens-config))


;;; Auto complete
(use-package auto-complete
  :ensure t
  :init
  (ac-config-default)
  (global-auto-complete-mode t))

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

;; Skewer -- live web development minor mode.
;; Methods of launching:
;;   * M-x run-skewer
;;   * Use the Skewer bookmarklet to inject it into an existing page.
;; Keybindings resemble the Lisp ones:
;;   C-x C-e -- JS: eval form (with prefix: insert result); CSS: load declaration.
;;   C-M-x -- JS: eval top-level-form; CSS: load rule; HTML: load tag.
;;   C-c C-k -- JS, CSS: eval buffer.
;;   C-c C-z -- JS: switch to REPL (logging: "skewer.log()", like "console.log()").
;; Forms are sent to all attached clients simultaneously (use `list-skewer-clients' to show them).
;; If the browser disconnects, use "skewer()" in the browser console to reconnect.
(use-package skewer-mode
  :ensure t
  :init
  (skewer-setup)) ; Integrate with js2-mode, html-mode and css-mode. (Don't worry about performance, this function is in a separate file.)

;; Auto-complete support (also provides jump-to-definition).
(use-package ac-js2
  :load-path "site-lisp/ac-js2"
  :init
  ;; TODO This does not seem to work, need to fix it
  ;; See https://github.com/ScottyB/ac-js2/issues/18
  (add-hook 'js2-mode-hook #'ac-js2-mode))

(use-package js2-refactor
  :ensure t
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config 
  (js2r-add-keybindings-with-prefix "C-c C-m")
  ;; eg. extract function with `C-c C-m ef`.
  )

;; TODO Add tern for emacs

(use-package json-mode
  :ensure t)

;;;;;;;;; F#

;; TODO Setup autocomplete for F# properly
(use-package fsharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
  (setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
  (setq fsharp-compiler "/usr/local/bin/fsharpc"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This provides customized support for writing programs in different kinds
;;;; of programming languages.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Inline emacs eval, From emacs.wordpress.com
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; This is how emacs tells the file type by the file suffix.
(setq auto-mode-alist
      (append '(("\\.mss$"         . scribe-mode))
              '(("\\.bib$"         . bibtex-mode))
              '(("\\.tex$"         . latex-mode))
              '(("\\.obj$"         . lisp-mode))
              '(("\\.st$"          . smalltalk-mode))
              '(("\\.Z$"           . uncompress-while-visiting))
              '(("\\.cs$"          . indented-text-mode))
              '(("\\.C$"           . c++-mode))
              '(("\\.cc$"          . c++-mode))
              '(("\\.icc$"         . c++-mode))
              '(("\\.c$"           . c-mode))
              '(("\\.y$"           . c-mode))
              '(("\\.h$"           . c++-mode))
              '(("\\.markdown$"    . markdown-mode))
              '(("\\.md$"          . markdown-mode))
              '(("\\.mkd$"         . markdown-mode))
              '(("\\.handlebars$"  . html-mode))
              '(("\\.json$"        . json-mode))
              '(("\\.scss$"        . css-mode))
              '(("\\.less$"        . css-mode))
              auto-mode-alist))

;; temporary hack for cases when not modifying whitespace is important
;; (defun delete-trailing-whitespace ()
;;   (interactive))
;; (setq standard-indent 8)
;; (setq-default indent-tabs-mode 1)
;; (defun untabify (start end)
;;   (interactive))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-hook 'csharp-mode-hook 'omnisharp-mode) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ob-fsharp
  :load-path "site-lisp/")

;; org-babel

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (fsharp . t)
   (js . t)))

(setq org-babel-python-command "python3")

(provide 'programming)
