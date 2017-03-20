;; Display preferences

;; Theme
;; Load and Set the colour theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Use "Fira Code" as the default font
;; 'light' weight looks best on mac
(set-frame-font "Fira Code-14:light")
;; Set some face attributes. This is not yet what I want.
;; I want to remove bold where possible.
(set-face-attribute 'default nil :font "Fira Code-14:light")             ;; default
(set-face-attribute 'mode-line nil :font "Fira Code-14:light")
(set-face-attribute 'mode-line-emphasis nil :font "Fira Code-14:light")  ;; Mode line should not be too bold
(set-face-attribute 'mode-line-buffer-id nil :font "Fira Code-14:light") ;; Mode line should not be too bold


;; This is specific to the mituharu/emacs-mac port
;; Enables fira code's ligatures
;; Via https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;; Highlights the current cursor line
(global-hl-line-mode t)

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
