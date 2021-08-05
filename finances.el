;; -*- lexical-binding: t; -*-

;; ==== Ledger mode ===

(use-package ledger-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode)))

(use-package flycheck-ledger
  :ensure t
  :config
  (eval-after-load 'flycheck
  '(require 'flycheck-ledger)))

;; === Beancount mode ===

;; Since beancount.el isn't on melpa, loading it from a local path

(defvar beancount-mode-path "~/opt/beancount-mode")

(if (file-directory-p beancount-mode-path)
    (progn
      (add-to-list 'load-path beancount-mode-path)
      (require 'beancount)
      (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
      (add-hook 'beancount-mode-hook #'outline-minor-mode)
      ))

(provide 'finances)
