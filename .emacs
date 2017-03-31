

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
 '(inhibit-startup-echo-area-message "ankit")
 '(package-selected-packages
   (quote
    (ws-butler hydra pandoc-mode ox-pandoc visible-mark org-bullets emmet-mode yaml-mode aggressive-indent ob-fsharp smartparens json-mode markdown-mode fsharp-mode ledger-mode hungry-delete auto-complete expand-region js2-mode multiple-cursors zenburn-theme which-key use-package try smex deft))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
