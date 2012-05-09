;; Create a symbolic link to open this file via the path ~/.emacs

;(set-default-font "-unknown-Droid Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(set-default-font "-unknown-Anonymous Pro-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

(if "~/config/elisp/init.el" 
    (load "~/config/elisp/init.el" t t))

;; End of file.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(eshell-save-history-on-exit t t)
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-echo-area-message "Ankit")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-silently-savep t)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline nil)
 '(markdown-command "/usr/local/bin/markdown -x footnotes")
 '(markdown-command-needs-filename t)
 '(markdown-css-path "file:///home/ankit/config/projects/doc-css/style.css")
 '(org-export-html-coding-system (quote utf-8))
 '(org-export-html-use-infojs nil)
 '(org-hide-leading-stars t)
 '(php-mode-force-pear t)
 '(python-continuation-offset 2)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(user-mail-address "ankit.solanki@gmail.com"))
;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;'(js2-comment-face ((t (:background "dim gray"))))
 ;'(js2-string-face ((t (:foreground "light blue")))))

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

;(text-scale-decrease 2)


(put 'narrow-to-region 'disabled nil)
