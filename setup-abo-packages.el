(use-package ivy

  :ensure t
  :diminish ivy-mode

  :config
  (ido-mode -1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; Show recently killed buffers when calling `ivy-switch-buffer'
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
  (use-package ivy-hydra :ensure t)

  :bind
  ("C-c C-r" . ivy-resume)
  ("C-x b"   . ivy-switch-buffer)

  )


(use-package counsel
  :ensure t
  :bind
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c k"   . counsel-rg)
  ("C-c i"   . counsel-imenu))

(setq counsel-rg-base-command "/opt/homebrew/bin/rg -S --no-heading --line-number --color never %s .")

;; (use-package swiper
;;   :ensure t
;;   :bind
;;   ("C-s" . swiper))

(provide 'setup-abo-packages)
