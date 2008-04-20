;; Auto insert matching brackets
(setq skeleton-pair t)
(local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(local-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; xml-mode
(add-to-list 'load-path "~/config/elisp/nxml-mode")
(autoload 'nxml-mode "nxml-mode" nil t)
(defalias 'xml-mode 'nxml-mode)

;; javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("js$" . js2-mode))

;; ===== Set standard indent to 2 rather that 4 ====
(setq standard-indent 2)

;; ===== Turn off tab character =====
;(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This provides customized support for writing programs in different kinds
;;;; of programming languages.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load the Python mode for .py

(add-to-list 'load-path "~/config/elisp/python-mode")
(autoload 'python-mode "python-mode" "Python editing mode." t)
    (setq auto-mode-alist
           (cons '("\\.py$" . python-mode) auto-mode-alist))
     (setq interpreter-mode-alist
           (cons '("python" . python-mode) interpreter-mode-alist))

;; Setup SLIME
;; (add-to-list 'load-path "d:/usr/emacs/slime-2.0/")  ; your SLIME directory
;; (setq inferior-lisp-program "d:/usr/bin/clisp-2.41/clisp.exe") ; your Lisp system
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)

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

(require 'php-mode)

;; This is how emacs tells the file type by the file suffix.
(setq auto-mode-alist
      (append '(("\\.mss$" . scribe-mode))
              '(("\\.bib$" . bibtex-mode))
              '(("\\.tex$" . latex-mode))
              '(("\\.obj$" . lisp-mode))
              '(("\\.st$"  . smalltalk-mode))
              '(("\\.Z$"   . uncompress-while-visiting))
              '(("\\.cs$"  . indented-text-mode))
              '(("\\.C$"   . c++-mode))
              '(("\\.cc$"  . c++-mode))
              '(("\\.icc$" . c++-mode))
              '(("\\.c$"   . c-mode))
              '(("\\.y$"   . c-mode))
              '(("\\.php$"   . php-mode))
              '(("\\.h$"   . c++-mode))
              auto-mode-alist))

;; Provide *magic* expansion for text ;
(global-set-key "\M- " 'hippie-expand)

;; Rails
;(add-to-list 'load-path "~/config/elisp/ruby-mode")
;(add-to-list 'load-path "~/config/elisp/emacs-rails")
;(require 'rails)

;; temporary hack for cases when not modifying whitespace is important
;; (defun delete-trailing-whitespace ()
;;   (interactive))
;; (setq standard-indent 8)
;; (setq-default indent-tabs-mode 1)
;; (defun untabify (start end)
;;   (interactive))

;; Follow PEAR coding guidelines for PHP
(defun php-mode-hook ()
  (setq tab-width 4
        c-basic-offset 4
        c-hanging-comment-ender-p nil
        indent-tabs-mode
        (not
         (and (string-match "/\\(PEAR\\|pear\\)/" (buffer-file-name))
              (string-match "\.php$" (buffer-file-name))))))

;; c# mode
(require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Follow Mono guidelines for C#
(defun my-csharp-mode-hook ()
  (message "CSharp Mode")
  (setq tab-width 8
        standard-indent 8
        c-basic-offset 8
        c-hanging-comment-ender-p nil
        indent-tabs-mode t))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(provide 'programming)
