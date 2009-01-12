;;; posts.el — A collection of hacks for easier blog posting.
;;
;; Author: Ankit Solanki <http://simulacra.in>
;; Uses: markdown-mode by Jason Blevins <http://jblevins.org/projects/markdown-mode/>

(defgroup posts nil
  "Blog posting stuff."
  :group 'languages)

(add-to-list 'load-path "~/config/elisp/markdown-mode")
(require 'markdown-mode)

;;;;;;;;;;;;;; Command paths ;;;;;;;;;;;;;;

(defcustom markdown-command-string "markdown"
  "Shell command for Markdown filter"
  :type 'string
  :group 'posts)

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


(defun blog-post-setup ()
  "This is not a mode, just some customizations that are applied
on top of markdown-mode each time a blog post is created."

  ; Start markdown-mode
  (markdown-mode)

  ; If longline-mode is present, use it.
  (if (boundp 'longlines-mode)
    (longlines-mode t)
    (auto-fill-mode -1))

  ; Set some custom key bindings
  (local-set-key "\C-cm" 'markdown)
  (local-set-key "\C-cp" 'markdown-preview)
  (local-set-key "\C-cs" 'ispell)
  (local-set-key "\C-cw" 'wordnet-current-word)

  (local-set-key "`" 'ong-insert-special))


;;; Amazon

(defcustom amazon-associates-code "simulacra-20"
  "Associates code for Amazon.com"
  :type 'string
  :group 'posts)

(defun add-amazon-link (isbn) 
  "Read an ISBN, and insert a link to Amazon.com for that ISBN with the appropriate associate code"
  (interactive "sISBN: ")
  (insert (concat "http://amazon.com/o/asin/" isbn "/ref=nosim/" amazon-associates-code)))

(provide 'posts)
