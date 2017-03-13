(require 'package)
;(add-to-list 'package-archives
;    '("marmalade" .
;      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; ========== Place Backup Files in Specific Directory ==========
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups/"))))
(setq delete-old-versions t)

;; ===== Make Text mode the default mode for new buffers =====
(setq major-mode 'text-mode)

;; ====== Add enhancements, remove annoyances ======

;; isearch + occur
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; ==== Load other libraries ====

;; Load and Set the colour theme
;(require 'color-theme)
;(setq load-path (cons "~/opt/elisp/emacs-color-theme-solarized" load-path))
;(require 'color-theme-solarized)
;(color-theme-charcoal-black)
(load-theme 'zenburn t)

;; spell check
(setq-default ispell-program-name "aspell")

;; Open simulacra
(defun simulacra ()
  (interactive)
  (find-file "/ssh:anks@simulacra.in:/home/.nest/anks/simulacra.in/"))

;; enable history of recent files
(recentf-mode t)

;; anything config
;(require 'anything-config)

;; (setq anything-sources (list anything-c-source-buffers
;;                              anything-c-source-file-name-history
;;                              anything-c-source-complex-command-history
;;                              anything-c-source-imenu
;;                              anything-c-source-emacs-commands
;;                              anything-c-source-locate))

;; (setq anything-filtered-candidate-transformers
;;       '((buffer   . anything-c-adaptive-sort)
;;         (file     . anything-c-adaptive-sort)
;;         (command  . anything-c-adaptive-sort)
;;         (function . anything-c-adaptive-sort)
;;         (sexp     . anything-c-adaptive-sort)))

;; (global-set-key (kbd "<C-menu>") 'anything)

(fset 'ank-add-quotes
   [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g tab return ?^ return ?> return])

;; make emacs use the clipboard
;(setq x-select-enable-clipboard t)
;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(require 'deft)
(global-set-key [f8] 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/deft")
(setq deft-text-mode 'markdown-mode)

;; Customise deft to remove file-vars from the titile line.
;; I like using org-mode in some long-form files, but deft displays the
;; file vars (-*- mode: org; -*-) in the title.
;; This custom function strips the file vars and gives a clean title.
(defun deft-title-fn-strip-file-vars (str)
  (replace-regexp-in-string "-\\*-.*-\\*-" "" (deft-strip-title str)))
(setq deft-parse-title-function 'deft-title-fn-strip-file-vars)

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)


(message "Loaded init file.")
(provide 'ankit-init)

