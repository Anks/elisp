;; Set font to consolas
;;(set-default-font "-*-Consolas-normal-r-normal-normal-14-90-120-120-c-*-fontset-uni")

;; Display preferences
(setq inhibit-startup-message t)
(menu-bar-mode nil)
(toggle-scroll-bar nil)
(tool-bar-mode nil)
(blink-cursor-mode nil)
(line-number-mode 1)
(column-number-mode 1)
(setq-default fill-column 72)
(global-font-lock-mode t)

;; ===== Automatically load abbreviations table =====
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/.abbrevs/.abbrev_defs")
(setq save-abbrevs t)

;; ========== Place Backup Files in Specific Directory ==========
;; Disable file backups
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups/"))))
(setq delete-old-versions t)

;; ===== Make Text mode the default mode for new buffers =====
(setq default-major-mode 'text-mode)
; Text-based modes have longlines-mode enabled
(add-hook 'text-mode-hook 'longlines-mode)

;; ====== Add enhancements, remove annoyances ======
(iswitchb-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key "\C-z" 'eshell)
(setq eshell-save-history-on-exit t)
(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "<up>") 'previous-line)
             (local-set-key (kbd "<down>") 'next-line)
             (local-set-key (kbd "C-z") 'bury-buffer)))

; isearch + occur
(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; ==== Load other libraries ====

;; Load HTMLize module
(require 'htmlize)

;; Load longlines module
(require 'longlines)

;; Load and Set the colour theme
(require 'color-theme)
(color-theme-charcoal-black)

;; Set proper encodings... UTF-8 all the way
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;
; Configure org mode ;
;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; spell check
(setq-default ispell-program-name "aspell")

;; Setup Tramp mode to use PuTTY
;;(setq tramp-default-method "pscp")

;; Open simulacra
(defun simulacra ()
  (interactive)
  (find-file "/ssh:anks@simulacra.in:/home/.nest/anks/simulacra.in/"))

;; Post
(defun post ()
  "Create a new blog post."
  (interactive)
  (let ((title (read-string "Post title? ")))
    (progn
      (find-file (concat "~/writing/posts/" title ".post"))
      (insert title)
      (newline)
      (insert "--------------------------------------")
      (newline)
      (newline))))

;; enable history of recent files
(recentf-mode t)

;; anything config
(require 'anything-config)


(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-file-name-history
                             anything-c-source-emacs-commands
                             anything-c-source-locate))

(setq anything-filtered-candidate-transformers
      '((buffer   . anything-adaptive-sort)
        (file     . anything-adaptive-sort)
        (command  . anything-adaptive-sort)
        (function . anything-adaptive-sort)
        (sexp     . anything-adaptive-sort)))

(global-set-key (kbd "<C-menu>") 'anything)

(fset 'ank-grep-for-text
   [?\C-z ?g ?r ?e ?p ?  ?- ?i ?H ?R ?\S-  ?\" ?\C-y ?\" ?  ?* return])

(fset 'ank-add-quotes
   [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g tab return ?^ return ?> return])

(server-start)

(message "Loaded init file.")

(provide 'ankit-init)
