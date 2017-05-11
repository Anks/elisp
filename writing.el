;;; Writing & General Text Editing related hacks

;; Spell checking
(use-package flyspell
  :diminish flyspell-mode
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode))

(use-package pandoc-mode
  :ensure t
  :config
  ;; Enable pandoc mode for org and markdown files
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'org-mode-hook 'pandoc-mode)

  ;; Unbind the default keybinding: C-c / is org-sparse-tree in org mode
  (define-key pandoc-mode-map (kbd "C-c /") nil)

  ;; Set C-c p to run pandoc
  :bind (("C-c e" . pandoc-main-hydra/body)))

(setq ispell-dictionary "british")

;; Provide *magic* expansion for text ;
(bind-key "M-/" 'hippie-expand)

;; ws-butler for trimming extra whitespace
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode-hook 'ws-butler-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->"         . mc/mark-next-like-this)
	 ("C-<"         . mc/mark-previous-like-this)
	 ("C-c C-<"     . mc/mark-all-like-this)))


;; Expand Region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; CSV
(use-package csv-mode
  :ensure t)

;; enable history of recent files
(recentf-mode t)
(setq recentf-max-saved-items 2000)

(use-package deft
  :ensure t

  :config
  (setq deft-extensions '("txt" "md" "markdown" "org")
        deft-default-extension "org"
        deft-directory "~/Dropbox/deft")

  ;; Customise deft to remove file-vars from the titile line.
  ;; I like using org-mode in some long-form files, but deft displays the
  ;; file vars (-*- mode: org; -*-) in the title.
  ;; This custom function strips the file vars and gives a clean title.
  (defun deft-title-fn-strip-file-vars (str)
    (replace-regexp-in-string "-\\*-.*-\\*-" "" (deft-strip-title str)))
  (setq deft-parse-title-function 'deft-title-fn-strip-file-vars)

  :bind (([f8] . deft)))


;; Olivetti
(use-package olivetti
  :ensure olivetti
  :config
  (setf olivetti-body-width 80)
  (visual-line-mode)

  ;; This is not working -- need to check
  ;;(add-hook 'deft-open-file-hook 'olivetti-mode)
  )

(use-package writegood-mode
  :ensure t
  :defer t
  :init
  (add-hook 'org-mode-hook 'writegood-mode)
  (add-hook 'markdown-mode-hook 'writegood-mode))

;;;;;;;;;;;;;; Smart quotes  ;;;;;;;;;;;;;;

;; Ongoing : http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs
;; Easy Insertion of Commonly-Used Special Characters

(defun one-quote () "" (interactive) (insert ?'))
(defvar sq-state 'nil "In single-quotes?")
(defvar dq-state 'nil "In double quotes?")
(defun ong-insert-special (c)
  "Insert special characters, like so:
 s => open/close single quotes
 d => open/close double quotes
 ' => apostrophe
 a => <a href=
 i => <img src=
 & => &amp;
 < => &lt;
 - => mdash
 . => horizontal ellipses"
  (interactive "c" "'")
  (cond
   ((= c ?s)
    (if sq-state
        (progn
          (insert-char #x2019)
          (setq sq-state 'nil))
      (insert-char #x2018)
      (setq sq-state 't)))
   ((= c ?d)
    (if dq-state
        (progn
          (insert-char #x201d)
          (setq dq-state 'nil))
      (insert-char #x201c)
      (setq dq-state 't)))
   ((= c ?') (insert-char #x2019))
   ((= c ?a)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<a href=\"\">")
      (backward-char 2)
      ))
   ((= c ?i)
    (progn
      (if (> (current-column) 0) (newline-and-indent))
      (insert "<img src=\"\" alt=\"\" />")
      (backward-char 11)
      ))
   ((= c ?&) (insert "&amp;"))
   ((= c ?<) (insert "&lt;"))
   ((= c ?-) (insert-char #x2014))
   ((= c ?`) (insert "`"))
   ((= c ?.) (insert-char #x002026))))

(bind-key "`" 'ong-insert-special)

(provide 'writing)
