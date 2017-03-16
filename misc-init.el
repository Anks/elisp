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

;; ==== Ledger mode ===

(use-package ledger-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

;; ==== Load other libraries ====

;; spell check
(setq-default ispell-program-name "aspell")

;; enable history of recent files
(recentf-mode t)

(use-package deft
  :ensure t

  :config
  (setq deft-extension "txt"
        deft-directory "~/Dropbox/deft"
        deft-text-mode 'markdown-mode)

  ;; Customise deft to remove file-vars from the titile line.
  ;; I like using org-mode in some long-form files, but deft displays the
  ;; file vars (-*- mode: org; -*-) in the title.
  ;; This custom function strips the file vars and gives a clean title.
  (defun deft-title-fn-strip-file-vars (str)
    (replace-regexp-in-string "-\\*-.*-\\*-" "" (deft-strip-title str)))
  (setq deft-parse-title-function 'deft-title-fn-strip-file-vars)

  :bind (([f8] . deft)))


;; which key
;; Shows a help popup when you press the first half of a keybinding
;; Added Mar '17
;; Not sure if it will stay for long term
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))
(server-start)


(message "Loaded init file.")
(provide 'misc-init)
