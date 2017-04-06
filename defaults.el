;; Some of these are from magnar's sane-defaults.el
;; Also from technomancy/better-defaults

;; via https://www.reddit.com/r/emacs/comments/4j828f/til_setq_gcconsthreshold_100000000/d34gbsp/
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Remove all chrome
;; Do this early to avoid flashing
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(scroll-bar-mode -1)

;; Use Trash in dired, etc
(setq delete-by-moving-to-trash t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; saveplace
(require 'saveplace)
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)

;; savehist
(savehist-mode 't)

;; Use F5 to refresh buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Prevent stale bytecode from being loaded
(setq load-prefer-newer t)

;; Echo timing after everything is loaded
(add-hook 'after-init-hook
          (lambda ()
            (message "Emacs initialized in %s" (emacs-init-time))))

(provide 'defaults)
