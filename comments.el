;;;-*-EMACS-LISP-*-
;;;
;;; Copyright 1988, 1989, 1990, 1991, 1992, 1993, 1994 Leigh L. Klotz, Jr.
;;; All Rights Reserved.

;;; for M-X Comment
(defvar user-comment-mode/string-alist
  ;; Just using commend-end and comment-start isn't good enough.
  ;; start-string end-string end-placement-command reindent-command
  '((lisp-mode ";; " nil nil (lisp-indent-line))
    (emacs-lisp-mode ";; " nil nil (lisp-indent-line))
    (c-mode "/* " " */" (backward-char 3) (c-indent-command))
    (postscript-mode "% " nil nil (ps-tab))))

;;; Add change comment.  Don't put in comment markers if given ^U arg.
(defun comment (arg)
  "Insert a date/initials comment.  With arg, don't insert comment markets."
  (interactive "*P")
  (print arg)
  (if (null arg) (open-line 1))
  (let* ((strings (cdr (assoc major-mode user-comment-mode/string-alist)))
	 (start-string (nth 0 strings)) 
	 (end-string (nth 1 strings))
	 (end-placement-command (nth 2 strings))
	 (reindent-command (nth 3 strings)))
    (and (null arg) (if start-string (insert start-string)))
    (insert (format "[%s] " (user-initials)))
    (insert-date)
    (and (null arg) (if end-string (insert end-string)))
    (and (null arg) (if end-placement-command (eval end-placement-command)))
    (if reindent-command (eval reindent-command))))


(defun user-initials ()
  (let ((prev ? ))
    (mapconcat '(lambda (c)
		  (cond ((= prev ? )  
			 (if (= c ? ) ""
			   (progn (setq prev c) (make-string 1 c))))
			(t (setq prev c) "")))
	       (user-full-name)
	       "")))

;;;;;;;;;;;;;;;;
; Box Comments ;
;;;;;;;;;;;;;;;;
(defun box-comment ()
  "Make a box comment of the current line."
  (interactive "*")
  (let* ((comment-start
	  (substring comment-start 0 (string-match "[ 	]+" comment-start)))
	 (comment-end
	  (if (zerop (length comment-end))
		   comment-start
		 (substring comment-end
			    (1+ (string-match "[ 	]+" comment-end)))))
	 (border
	  (substring comment-start (1- (length comment-start)))))
    (beginning-of-line 1)
    (insert comment-start " ")
    (capitalize-word 1)
    (end-of-line 1)
    (insert " " comment-end)
    (let ((width (current-column)))
      (insert "\n")
      (beginning-of-line 0)
      (insert-box-border width border)
      (insert "\n")
      (beginning-of-line 2)
      (insert-box-border width border))))

(defun insert-box-border (len char)
  "Make a top or bottom border of a box comment."
  (insert comment-start)
  (let ((i (- len (+ (length comment-start) (length comment-end)))))
    (while (not (zerop i))
      (setq i (1- i))
      (insert char)))
  (insert comment-end))

