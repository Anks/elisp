;; Auto insert matching brackets
(setq skeleton-pair t)
(local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "<") 'skeleton-pair-insert-maybe)
(show-paren-mode t)

;; YASnippet
;(require 'yasnippet)
;(yas-global-mode 1)

(require 'move-text)
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; xml-mode
(autoload 'nxml-mode "nxml-mode" nil t)
(defalias 'xml-mode 'nxml-mode)

(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; javascript mode
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("js$" . js2-mode))

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(defun my-js2-mode-hook ()
  (require 'js2-refactor)
  ;(auto-fill-mode t)
  )

(setq js2-global-externs '("require" "module" "console"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

(require 'fsharp-mode)
(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")
(setq fsharp-ac-debug 0)

;; (add-to-list 'load-path "~/opt/elisp/site-lisp/tern/emacs")
;; (autoload 'tern-mode "tern.el" nil t)
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (eval-after-load 'auto-complete
;;   '(eval-after-load 'tern
;;      '(progn
;;         (require 'tern-auto-complete)
;;         (tern-ac-setup))))

;; ===== Set standard indent to 2 rather that 4 ====



(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; Set the tab width
(setq tab-width 2)
(setq c-basic-indent 2)

;; end via

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
              '(("\\.json$"        . js2-mode))
              '(("\\.scss$"        . css-mode))
              '(("\\.less$"        . css-mode))
              auto-mode-alist))

;; Provide *magic* expansion for text ;
(global-set-key "\M- " 'hippie-expand)

;; temporary hack for cases when not modifying whitespace is important
;; (defun delete-trailing-whitespace ()
;;   (interactive))
;; (setq standard-indent 8)
;; (setq-default indent-tabs-mode 1)
;; (defun untabify (start end)
;;   (interactive))


(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'ob-fsharp)

(provide 'programming)
