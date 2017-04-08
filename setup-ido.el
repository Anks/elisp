
;; Use ido
(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq
   ido-enable-flex-matching t
   ido-use-virtual-buffers t
   ido-create-new-buffer (quote never)
   ido-enable-last-directory-history t
   ido-case-fold t
   ido-enable-prefix nil
   ))


(add-hook
 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (cond
        ((looking-back "~/") (insert "work/"))
        ((looking-back "/") (insert "~/"))
        (:else (call-interactively 'self-insert-command)))))

   ;; Use C-w to go back up a dir to better match normal usage of C-w
   ;; - insert current file name with C-x C-w instead.
   (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
   (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-use-faces t)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(provide 'setup-ido)
