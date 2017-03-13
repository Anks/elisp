;; Install
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

;; Exports
(provide 'setup-smex)
