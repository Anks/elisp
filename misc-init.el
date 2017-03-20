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
