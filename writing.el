;;; Writing & General Text Editing related hacks

;; Provide *magic* expansion for text ;
(global-set-key "\M- " 'hippie-expand)

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

(global-set-key "`" 'ong-insert-special)

(provide 'writing)
