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

(defun anks/uni-insert (name)
  "Insert a unicode character by NAME."
  (insert-char (cdr (assoc-string name (ucs-names)))))

(defvar anks/unicode-toggle-state-hash
  (make-hash-table :test 'equal))

(defun anks/insert-char-toggle (char1 char2)
  "Toggle-insert. Insert CHAR1 or CHAR2 into buffer each time it's called."
  (let
      ((hash-key (concat char1 char2)))
    (let ((hash-value (gethash hash-key anks/unicode-toggle-state-hash)))
      (if (not hash-value)
          (anks/uni-insert char1)
        (anks/uni-insert char2))
      (puthash hash-key (not hash-value) anks/unicode-toggle-state-hash))))


(defhydra hydra-anks/insert-unicode (:hint nil :color blue)
  "
_-_ Em dash        _s_ Single quote, toggle     _<up>_ Thumbs up
_=_ En dash        _d_ Double quote, toggle     _<down>_ Thumbs down
_._ Ellipsis       _'_ Curly apostrophe

_1_ Superscript 1  _x_ Multiplication sign
_2_ Superscript 2  _c_ Check Mark
  …                _b_ Bullet

  "

  ("`" (anks/uni-insert "GRAVE ACCENT"))
  ("-" (anks/uni-insert "EM DASH"))
  ("=" (anks/uni-insert "EN DASH"))
  ("." (anks/uni-insert "HORIZONTAL ELLIPSIS"))
  ("x" (anks/uni-insert "MULTIPLICATION SIGN"))
  ("'" (anks/uni-insert "RIGHT SINGLE QUOTATION MARK")) ;; iffy
  ("c" (anks/uni-insert "CHECK MARK"))
  ("b" (anks/uni-insert "BULLET"))

  ("s" (anks/insert-char-toggle
        "LEFT SINGLE QUOTATION MARK"
        "RIGHT SINGLE QUOTATION MARK"))
  ("d" (anks/insert-char-toggle
        "LEFT DOUBLE QUOTATION MARK"
        "RIGHT DOUBLE QUOTATION MARK"))

  ("1" (anks/uni-insert "SUPERSCRIPT DIGIT ONE"))
  ("2" (anks/uni-insert "SUPERSCRIPT DIGIT TWO"))
  ("3" (anks/uni-insert "SUPERSCRIPT DIGIT THREE"))
  ("4" (anks/uni-insert "SUPERSCRIPT DIGIT FOUR"))
  ("5" (anks/uni-insert "SUPERSCRIPT DIGIT FIVE"))
  ("6" (anks/uni-insert "SUPERSCRIPT DIGIT SIX"))
  ("7" (anks/uni-insert "SUPERSCRIPT DIGIT SEVEN"))
  ("8" (anks/uni-insert "SUPERSCRIPT DIGIT EIGHT"))
  ("9" (anks/uni-insert "SUPERSCRIPT DIGIT NINE"))

  ("<up>"   (anks/uni-insert "THUMBS UP SIGN"))
  ("<down>" (anks/uni-insert "THUMBS DOWN SIGN"))
  )


(bind-key "`" 'hydra-anks/insert-unicode/body)

(provide 'writing)
