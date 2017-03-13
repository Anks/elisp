
;; Initialization

;;; Directory / File setup
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/notes.org")
(setq org-archive-location "~/Dropbox/org/.archive")
(setq org-agenda-files (list org-directory))

;;; Behaviour customization
(setq org-hide-leading-stars 't)
(setq org-special-ctrl-a 't)
(setq org-special-ctrl-e 't)
(setq org-return-follows-link 't)

;;; Configure org-capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Tasks")
         "* TODO %?\n  %i\n SCHEDULED %t")
        ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; Key Bindings
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Exports
(provide 'setup-org)
