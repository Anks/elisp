;; Display preferences

(set-face-attribute 'default nil :font "Source Code Pro ExtraLight-15")
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Theme
;; Load and Set the colour theme
(use-package zenburn-theme :ensure t)
(load-theme 'zenburn t)

;; Remove all chrome
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

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

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; TODO Move to hydra
;; ;; Easy zoom-in and zoom-out via <f2>g & <f2>l
;; ;; via http://oremacs.com/2015/01/14/repeatable-commands/
;; (defun def-rep-command (alist)
;;   "Return a lambda that calls the first function of ALIST.
;; It sets the transient map to all functions of ALIST."
;;   (lexical-let ((keymap (make-sparse-keymap))
;;                 (func (cdar alist)))
;;     (mapc (lambda (x)
;;             (define-key keymap (car x) (cdr x)))
;;           alist)
;;     (lambda (arg)
;;       (interactive "p")
;;       (funcall func arg)
;;       (set-transient-map keymap t))))

;; (global-set-key (kbd "<f2> g")
;;                 (def-rep-command
;;                     '(("g" . text-scale-increase)
;;                       ("l" . text-scale-decrease))))
;; (global-set-key (kbd "<f2> l")
;;                 (def-rep-command
;;                     '(("l" . text-scale-decrease)
;;                       ("g" . text-scale-increase))))

(provide 'appearance)
