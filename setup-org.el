
;; Initialization

;;; Directory / File setup
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-archive-location "~/Dropbox/org/.archive")
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
         "* TODO %?\n  %i\n SCHEDULED: %t")
        ("d" "Deadline" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
         "* TODO %?\n  %i\n DEADLINE: %t")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Key Bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

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


;; Exports
(provide 'setup-org)
