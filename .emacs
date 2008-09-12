;; Create a symbolic link to open this file via the path ~/.emacs

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
 '(eshell-save-history-on-exit t)
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-echo-area-message "Ankit")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-silently-savep t)
 '(org-export-html-style "<style type=\"text/css\">
  html {
	font-family: Times, serif;
	font-size: 12pt;
  }
  .title { text-align: center; }
  .todo, .deadline { color: red; }
  .done { color: green; }
  .target { background-color: lavender; }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
  }
  table { border-collapse: collapse; }
  td, th {
	vertical-align: top;
	border: 1pt solid #ADB9CC;
  }
* { font-family: palatino, georgia, 'bitstream vera serif', serif; font-weight: normal; background: #fff; color: #000; }
body { margin: 80pt; }
a { border-bottom: 1pt solid #999; text-decoration: none; }
address, cite, var, abbr { font-style: normal; }
blockquote { margin-left: 2em; padding: 0 .5em; border-left: 1pt solid #999;}
caption, th, td { border-bottom: 1pt solid #ddd; }
dd p, dd pre { margin-top: 0; }
dt { margin: .6em 0 0; }
form { display: none; }
html { font-size: 100%; line-height: 150%; }
p, address, blockquote, ul, ol, dl, table, pre { margin: .6em 0; }
pre { overflow: auto; padding: 2px 3px; }
strong, strong *, em *,  var, kbd { font-style: italic !important; }
table { border-collapse: collapse; width: 100%; }
code { font-family: 'bitstream vera sans mono', consolas, 'courier new';
font-size: 0.9em; background-color: #eee;}
ol { margin-left: 1em; padding-left: 0;}
h4 { font-weight: bold;}
</style>")
 '(org-hide-leading-stars t)
 '(php-mode-force-pear t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(transient-mark-mode t)
 '(user-mail-address "ankit.s@imfinity.com"))
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
