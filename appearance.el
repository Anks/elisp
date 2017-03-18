;; Display preferences

(set-face-attribute 'default nil :font "Source Code Pro ExtraLight-15")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;; Highlights the current cursor line
(global-hl-line-mode t)

;; Theme
;; Load and Set the colour theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Do not blink the cursor
(blink-cursor-mode -1)

;; Always show line and column numbers
(line-number-mode 1)
(column-number-mode 1)

;; Keep default fill column of 72
(setq-default fill-column 72)

;; This is only for old emacsen
(global-font-lock-mode t)

;; Turn off the beep completely
(setq ring-bell-function 'ignore)

(require 'diminish nil t)
(provide 'appearance)
