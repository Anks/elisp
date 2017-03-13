(defun ank-unicode-insert ()
  (interactive)
  (let* ((string "")
        (keywords (read-string "Enter unicode name (approx): "))
        (completion (car (ank-get-unicode-results keywords))))
    (dotimes (i 10)
      (let ((name (cadr (nth i completion))))
        (if (not (null name))
            (setf string 
                  (concatenate 'string string "(" (number-to-string i) "): " name " ")))))
    (insert-char (car (nth (read-number string 0) completion)))))

(defun ank-get-unicode-results (keywords) 
  (read-from-string
   (shell-command-to-string 
    (concat "python g:/ankit/home/projects/unicode/unicode.py " keywords))))
