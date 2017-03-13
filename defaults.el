;; Some of these are from magnar's sane-defaults.el
;; Also from technomancy/better-defaults

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

;; Use F5 to refresh buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Prevent stale bytecode from being loaded
(setq load-prefer-newer t)

(provide 'defaults)
