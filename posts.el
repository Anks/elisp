;;; posts.el — A collection of hacks for easier blog posting.
;;
;; Author: Ankit Solanki <ankit@simulacra.in>
;;

(defgroup posts nil
  "Blog posting stuff."
  :group 'languages)


;;;;;;;;;;;;;; Command paths ;;;;;;;;;;;;;;

(defcustom markdown-command-string "markdown"
  "Shell command for Markdown filter"
  :type 'string
  :group 'posts)

(defcustom smartypants-command-string "perl D:\\bin\\Markdown\\SmartyPants.pl"
  "Shell command for Smartypants filter"
  :type 'string
  :group 'posts)

(defcustom textile-command-string "python -u D:\\bin\\textile-2.0.10\\run.py"
  "Shell command for Textile filter"
  :type 'string
  :group 'posts)

(defcustom add-quotes-command-string "python -u D:\\bin\\addQuote\\addQuote.py"
  "Shell command for the 'add-quotes' filter"
  :type 'string
  :group 'posts)

;;;;;;;;;;;;;; Generic functions to filter text ;;;;;;;;;;;;;;

(defun clear-buffer (buf)
  "Clear a buffer"
  (save-excursion
    (set-buffer buf)
    (kill-region (point-min) (point-max))))

(defun convert-buffer-text (command)
  "Run a text-to-HTML converter on the current buffer"
  (let ((buf (get-buffer-create "*post-preview*")))
    (clear-buffer buf)
    (shell-command-on-region (point-min) (point-max) command buf nil)
    (switch-to-buffer buf)
    (delete-other-windows)
    (html-mode)))

(defun convert-region-text (start end command)
  "Run a text-to-HTML converter on the current region"
  (let ((buf (get-buffer-create "*post-preview*")))
    (clear-buffer buf)
    (shell-command-on-region start end command buf nil)
    (switch-to-buffer buf)
    (delete-other-windows)
    (html-mode)))

(defun convert-text-to-html (command)
  "Run a text-to-HTML converter on the current region/buffer"
  (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
      (convert-region-text (region-beginning) (region-end) command)
    (convert-buffer-text command)))


;;;;;;;;;;;;;; Specific functions to filter text ;;;;;;;;;;;;;;

(defun markdown ()
  "Run Markdown on the current buffer. If the mark is active,
then run it on the current region. The HTML output is displayed in a
buffer named *post-preview*."
  (interactive)
  (convert-text-to-html markdown-command-string))

(defun markdown-preview ()
  "Converts the current buffer/region to HTML via Markdown, and then the output is
displayed in your default web browser.

Also see http://blog.goterkyourself.com/articles/2007/01/31/emacs-markdown-functions"
  (interactive)
  (markdown)
  (let ((buf (get-buffer-create "*post-preview*")))
    (browse-url-of-buffer buf)))

(defun textile ()
  "Run Textile on the current buffer. If the mark is active,
then run it on the current region. The HTML output is displayed in a
buffer named *post-preview*."
  (interactive)
  (convert-text-to-html textile-command-string))

;;;;;;;;;;;;;; Other misc utility functions ;;;;;;;;;;;;;;

(defun add-quote-to-region (start end)
  "Adds one level of quotation marks to selected text"
  (interactive "r")
  (shell-command-on-region start end add-quotes-command-string t t))

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
          (ucs-insert #x2019)
          (setq sq-state 'nil))
      (ucs-insert #x2018)
      (setq sq-state 't)))
   ((= c ?d)
    (if dq-state
        (progn
          (ucs-insert #x201d)
          (setq dq-state 'nil))
      (ucs-insert #x201c)
      (setq dq-state 't)))
   ((= c ?') (ucs-insert #x2019))
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
   ((= c ?-) (ucs-insert #x2014))
   ((= c ?`) (insert "`"))
   ((= c ?.) (ucs-insert #x002026))))

;;;;;;;;;;;; Wordnet ;;;;;;;;;;;;;;;;

(defun get-current-word ()
  "Returns the current, or the last entered word."
  (save-excursion
    (backward-word)
    (setq start (point))
    (forward-word)
    (setq end (point))
    (buffer-substring-no-properties start end)))

(defcustom wordnet-bin-path
  "wn"
  "This should point to the full path of the wordnet command"
  :type 'string
  :group 'posts)

(defun wordnet-current-word ()
  "Shows the Wordnet overview for the current word."
  (interactive)
  (save-window-excursion
    (let ((buf (get-buffer-create "*wordnet*"))
          (word (get-current-word)))
      (save-window-excursion
        (set-buffer buf)
        (clear-buffer buf)
        (insert (concat "Wordnet overview for " word ": "))
        (newline)
        (call-process wordnet-bin-path nil "*wordnet*" t word "-over")
        (switch-to-buffer "*wordnet*")
        (beginning-of-buffer)
        (read-string "Press Enter to continue… ")))))

(defvar etymonline-path "g:/ankit/home/projects/etymonline/etymonline.py")

(defun etymology-current-word ()
  "Shows the Etymology of the current word."
  (interactive)
  (save-window-excursion
    (let ((buf (get-buffer-create "*etymonline*"))
          (word (get-current-word)))
      (save-window-excursion
        (message
         (concat "Connecting to etymonline.com to get the etymology for “" word "”…"))
        (set-buffer buf)
        (clear-buffer buf)
        (insert (concat "Etymology of " word ": "))
        (newline)
        (call-process-shell-command "python" nil "*etymonline*" t etymonline-path word)
        (switch-to-buffer "*etymonline*")
        (beginning-of-buffer)
        (read-string "Press Enter to continue… ")))))

;;;;;;;;;;;;;; Initialization  ;;;;;;;;;;;;;;

(setq auto-mode-alist
      (cons '("\\.post$" . blog-post-setup) auto-mode-alist))



;;; Regular expressions =======================================================

;; Links
(defconst regex-link-inline "\\(!?\\[.*?\\]\\)\\(([^\\)]*)\\)"
  "Regular expression for a [text](file) or an image link ![text](file)")
(defconst regex-link-reference "\\(!?\\[.+?\\]\\)[ ]?\\(\\[.*?\\]\\)"
  "Regular expression for a reference link [text][id]")
(defconst regex-reference-definition
  "^ \\{0,3\\}\\(\\[.+?\\]\\):[ ]?\\(.*?\\)\\(\"[^\"]+?\"\\)?$"
  "Regular expression for a link definition [id]: ...")

;; itex/LaTeX
(defconst markdown-regex-latex-expression
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for itex $..$ or $$..$$ math mode expressions")

(defconst markdown-regex-latex-display
    "^\\\\\\[\\(.\\|\n\\)*?\\\\\\]$"
  "Regular expression for itex \[..\] display mode expressions")


;;; Font lock faces ==========================================================

;; Make new faces based on existing ones

;;; This is not available in Emacs 21 so it has been disabled until
;;; something can be built from scratch.  If you are running Emacs 22 and
;;; want to underline line breaks, uncomment this face and the associated
;;; regular expression below.

(copy-face 'nobreak-space 'markdown-font-lock-line-break-face)

(defface markdown-font-lock-bold-face '((t (:inherit bold)))
  "`markdown-mode' face used to highlight **bold** and __bold__ text.")
(defface markdown-font-lock-italic-face '((t (:inherit italic)))
  "`markdown-mode' face used to highlight *italic* and _italic_ text.")
(defface markdown-font-lock-inline-code-face '((t (:inherit fixed-pitch)))
  "`markdown-mode' face used to highlight `inline code` fragments.")

;;; If you prefer to highlight italic/bold/code using colors, rather than
;;; with italic and bold and fixed faces, uncomment the following lines.

;(copy-face 'font-lock-variable-name-face 'markdown-font-lock-italic-face)
;(copy-face 'font-lock-type-face 'markdown-font-lock-bold-face)
;(copy-face 'font-lock-builtin-face 'markdown-font-lock-inline-code-face)

(copy-face 'font-lock-function-name-face 'markdown-font-lock-header-face)
(copy-face 'font-lock-variable-name-face 'markdown-font-lock-list-face)
(copy-face 'font-lock-comment-face 'markdown-font-lock-blockquote-face)
(copy-face 'font-lock-constant-face 'markdown-font-lock-link-face)
(copy-face 'font-lock-type-face 'markdown-font-lock-reference-face)
(copy-face 'font-lock-string-face 'markdown-font-lock-url-face)
(copy-face 'font-lock-builtin-face 'markdown-font-lock-math-face)

;; Define the extra font lock faces
;(defvar markdown-font-lock-line-break-face 'markdown-font-lock-line-break-face
;  "Face name to use for line breaks.")
(defvar markdown-font-lock-italic-face 'markdown-font-lock-italic-face
  "Face name to use for italics.")
(defvar markdown-font-lock-bold-face 'markdown-font-lock-bold-face
  "Face name to use for bold.")
(defvar markdown-font-lock-header-face 'markdown-font-lock-header-face
  "Face name to use for headers.")
(defvar markdown-font-lock-inline-code-face 'markdown-font-lock-inline-code-face
  "Face name to use for inline code.")
(defvar markdown-font-lock-list-face 'markdown-font-lock-list-face
  "Face name to use for list items.")
(defvar markdown-font-lock-blockquote-face 'markdown-font-lock-blockquote-face
  "Face name to use for blockquotes and code blocks.")
(defvar markdown-font-lock-link-face 'markdown-font-lock-link-face
  "Face name to use for links.")
(defvar markdown-font-lock-reference-face 'markdown-font-lock-reference-face
  "Face name to use for references.")
(defvar markdown-font-lock-url-face 'markdown-font-lock-url-face
  "Face name to use for URLs.")
(defvar markdown-font-lock-math-face 'markdown-font-lock-math-face
  "Face name to use for itex expressions.")

(defconst markdown-mode-font-lock-keywords-basic
  (list
   ;;;
   ;;; Code ----------------------------------------------------------
   ;;;
   ;; Double backtick style ``inline code``
   (cons "``.+?``" 'markdown-font-lock-inline-code-face)
   ;; Single backtick style `inline code`
   (cons "`.+?`" 'markdown-font-lock-inline-code-face)
   ;; Four-space indent style code block
   (cons "^    .*$" 'markdown-font-lock-blockquote-face)
   ;;;
   ;;; Headers and Horizontal Rules ----------------------------------
   ;;;
   ;; Equals style headers (===)
   (cons ".*\n===+" 'markdown-font-lock-header-face)
   ;; Hyphen style headers (---)
   (cons ".*\n---+" 'markdown-font-lock-header-face)
   ;; Hash style headers (###)
   (cons "^#+ .*$" 'markdown-font-lock-header-face)
   ;; Asterisk style horizontal rules (* * *)
   (cons "^\\*[ ]?\\*[ ]?\\*[ ]?[\\* ]*$" 'markdown-font-lock-header-face)
   ;; Hyphen style horizontal rules (- - -)
   (cons "^-[ ]?-[ ]?-[--- ]*$" 'markdown-font-lock-header-face)
   ;;;
   ;;; Special cases -------------------------------------------------
   ;;;
   ;; List item including bold
   (cons "^\\s *\\* .*?[^\\\n]?\\(\\*\\*.*?[^\n\\]\\*\\*\\).*$"
         '(1 'markdown-font-lock-bold-face))
   ;; List item including italics
   (cons "^\\* .*?[^\\\n]?\\(\\*.*?[^\n\\]\\*\\).*$"
         '(1 'markdown-font-lock-italic-face))
   ;;;
   ;;; Lists ---------------------------------------------------------
   ;;;
   ;; Numbered lists (1. List item)
   (cons "^[0-9]+\\.\\s " 'markdown-font-lock-list-face)
   ;; Level 1 list item (no indent) (* List item)
   (cons "^\\(\\*\\|\\+\\|-\\) " '(1 'markdown-font-lock-list-face))
   ;; Level 2 list item (two or more spaces) (   * Second level list item)
   (cons "^  [ ]*\\(\\*\\|\\+\\|-\\) " 'markdown-font-lock-list-face)
   ;;;
   ;;; Links ---------------------------------------------------------
   ;;;
   (cons regex-link-inline '(1 'markdown-font-lock-link-face t))
   (cons regex-link-inline '(2 'markdown-font-lock-url-face t))
   (cons regex-link-reference '(1 'markdown-font-lock-link-face t))
   (cons regex-link-reference '(2 'markdown-font-lock-reference-face t))
   (cons regex-reference-definition '(1 'markdown-font-lock-reference-face t))
   (cons regex-reference-definition '(2 'markdown-font-lock-url-face t))
   (cons regex-reference-definition '(3 'markdown-font-lock-link-face t))
   ;;;
   ;;; Bold ----------------------------------------------------------
   ;;;
   ;; **Asterisk** and _underscore_ style bold
   (cons "[^\\]\\(\\(\\*\\*\\|__\\)\\(.\\|\n\\)*?[^\\]\\2\\)"
         '(1 'markdown-font-lock-bold-face))
   ;;;
   ;;; Italic --------------------------------------------------------
   ;;;
   ;; *Asterisk* and _underscore_ style italic
   (cons "[^\\]\\(\\(\\*\\|_\\)\\(.\\|\n\\)*?[^\\]\\2\\)"
         '(1 'markdown-font-lock-italic-face))
   ;;;
   ;;; Blockquotes ---------------------------------------------------
   ;;;
   (cons "^>.*$" 'markdown-font-lock-blockquote-face)
   ;;;
   ;;; Hard line breaks ----------------------------------------------
   ;;;
   ;; Trailing whitespace (two spaces at end of line)
;   (cons "  $" 'markdown-font-lock-line-break-face)
   )
  "Syntax highlighting for Markdown files.")

(defun blog-post-setup ()
  "This is not a mode, just some customizations that are applied
on top of html-mode each time a blog post is created."
  (html-mode)
  (if (boundp 'longlines-mode)
    (longlines-mode t)
    (auto-fill-mode -1))

  (local-set-key "\C-cm" 'markdown)
  (local-set-key "\C-cp" 'markdown-preview)
  (local-set-key "\C-cs" 'ispell)
  (local-set-key "\C-cq" 'add-quote-to-region)
  (local-set-key "\C-cw" 'wordnet-current-word)

  (local-set-key "`" 'ong-insert-special)
  (font-lock-add-keywords nil markdown-mode-font-lock-keywords-basic))


(provide 'posts)
