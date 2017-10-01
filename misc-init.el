;; https://www.masteringemacs.org/article/discoverel-discover-emacs-context-menus
(use-package discover
  :ensure t
  :config
  (global-discover-mode t))

;; Use F5 to refresh buffer
(bind-key "<f5>" 'revert-buffer)

;; ========== Place Backup Files in Specific Directory ==========
(setq make-backup-files t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/.backups/"))))
(setq delete-old-versions t)

;; === Helpful ===

(use-package helpful :ensure t
  :bind
  ("C-h f" . helpful-function)
  ("C-h v" . helpful-variable))


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

;; ==== Load other libraries ====

;; spell check
(setq-default ispell-program-name "aspell")

;; Via http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; This line actually replaces Emacs' entire narrowing
;; keymap, that's how much I like this command. Only
;; copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map "\C-xn"
              nil)))

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
