
;; Initialization

;;; Directory / File setup
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-archive-location "~/Dropbox/org/archive.org::datetree/")
(setq org-agenda-files (list org-directory))

;;; Behaviour customization
(setq org-special-ctrl-a 't)
(setq org-special-ctrl-e 't)
(setq org-return-follows-link 't)

;; hiding stars does not play well with Fira code ligatures
;; So using org-bullets instead
(setq org-hide-leading-stars nil)
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;; Export options
(setq org-export-html-postamble nil)

;;; TODO States
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "ONIT(o@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(setq org-todo-keyword-faces
      '(("WAIT" . "yellow") ("ONIT" . "ORANGE")))

;;; Configure org-capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
         "* TODO %?\n  SCHEDULED: %t\n  %i")
        ("d" "Deadline" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
         "* TODO %?\n  DEADLINE: %t\n  %i")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Via https://github.com/jkitchin/jmax/blob/master/org/org-show.org
;; Also see https://www.youtube.com/watch?v=JZ8RK-R9O_g
;; Do not automatically load. Load only when required
;; (require 'org-show)

;; Key Bindings
(bind-key "C-c c" 'org-capture)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c a" 'org-agenda)

;; TODO Figure out a good way to start a org-caputre workflow with fullscreen emacs
;; (use-package noflet
;;   :ensure t )

;; (defun make-capture-frame ()
;;   "Create a new frame and run org-capture."
;;   (interactive)
;;   (make-frame '((name . "capture")))
;;   (select-frame-by-name "capture")
;;   (delete-other-windows)
;;   (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
;;           (org-capture)))


;; org-babel
(setq org-src-fontify-natively t)

;; Exports
(provide 'setup-org)
