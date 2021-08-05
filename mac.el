
;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/opt/homebrew/bin/aspell")


;; Use mdfind instead of locate
(setq locate-command "mdfind")

;; (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
;;   (normal-top-level-add-subdirs-to-load-path))

;; Fix path in Mac
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(provide 'mac)
