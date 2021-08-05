;; -*- lexical-binding: t; -*-

;; Display preferences

;; Enable diminish mode
(use-package diminish
  :ensure t)

;; Theme
;; No theme currently, use defaults.


(use-package color-identifiers-mode
  :ensure t
  :defer t)

;; Use "Fira Code" as the default font
;; 'light' weight looks best on mac
(set-frame-font "Fira Code-12:light")
(set-face-attribute 'default nil :font "Fira Code-12:light")             ;; default face

;; Disables bold in mode line
(set-face-attribute 'mode-line nil :font "Fira Code-12:light")
(set-face-attribute 'mode-line-emphasis nil :font "Fira Code-12:light")  ;; Mode line should not be too bold
(set-face-attribute 'mode-line-buffer-id nil :font "Fira Code-12:light") ;; Mode line should not be too bold

;; Via http://stackoverflow.com/a/20693389
;; Disables bold in minibuffer
(progn
  (defun remap-faces-default-attributes ()
    (let ((family (face-attribute 'default :family)))
      (mapcar (lambda (face)
                (face-remap-add-relative
                 face :family family :weight 'light))
              (face-list))))

  (when (display-graphic-p)
    (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)))

;; This is specific to the mituharu/emacs-mac port
;; Enables fira code's ligatures
;; Via https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

;; Thin cursors
(setq-default cursor-type 'bar)

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
