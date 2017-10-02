

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(if "~/opt/elisp/init.el"
    (load "~/opt/elisp/init.el" t t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" default)))
 '(inhibit-startup-echo-area-message "ankit")
 '(package-selected-packages
   (quote
    (color-identifiers-mode tao-theme helpful editorconfig edit-indirect terraform-mode suggest org ivy-hydra avy counsel ivy magit ag flycheck-ledger hardcore-mode discover company-restclient ob-restclient restclient writegood-mode projectile ido-vertical-mode ido-vertical ws-butler hydra pandoc-mode ox-pandoc visible-mark org-bullets emmet-mode yaml-mode aggressive-indent ob-fsharp smartparens json-mode markdown-mode fsharp-mode ledger-mode hungry-delete auto-complete expand-region js2-mode multiple-cursors zenburn-theme which-key use-package try smex deft))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
