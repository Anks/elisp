(require 'actionscript-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.mxml$" . actionscript-mode))

(require 'compile)
;; Find error messages in flex compiler output:
(push '("^\\(.*\\)(\\([0-9]+\\)): col: \\([0-9]+\\) Error: \\(.*\\)$" 1 2 3 2) compilation-error-regexp-alist)


(require 'flymake)

(defvar as3-build-file nil)

(defvar as3-default-build-file-name "build_manage.rb")

(defun flymake-as3-mode (&optional file)
  (interactive 
   (list (read-file-name "Build file: " default-directory as3-default-build-file-name)))
  (message file)
  (flymake-mode 0)
  (let* ((build-file 
	  (if file (expand-file-name file)
	    (if as3-build-file as3-build-file
	      (expand-file-name (concat default-directory as3-default-build-file-name))))))
    (if (file-exists-p build-file)
	(progn
	  (setq as3-build-file build-file)
	  (flymake-mode 1)
	  (message (concat "Set flymake mode with build file, " build-file ".")))
      (message (concat "Build file, " build-file ", does not exist.")))))

(defun flymake-as3-init ()
  (if as3-build-file
      (progn
	(remove-hook 'after-save-hook 'flymake-after-save-hook t)
	(save-buffer)
	(add-hook 'after-save-hook 'flymake-after-save-hook nil t)
	(list "ruby" (list as3-build-file "compile")))))

(defun flymake-as3-cleanup () (message "Flymake finished checking AS3."))
(defun flymake-as3-get-real-file-name (tmp-file) tmp-file)


(setq flymake-allowed-file-name-masks
      (cons '(".+\\.as$\\|.+\\.mxml$"
	      flymake-as3-init
	      flymake-as3-cleanup
	      flymake-as3-get-real-file-name)
	    flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^\\(.*\\)(\\([0-9]+\\)): col: \\([0-9]+\\) Error: \\(.*\\)$" 1 2 3 4)
	    flymake-err-line-patterns))

(define-key actionscript-mode-map (kbd "C-c p") 'flymake-goto-prev-error)
(define-key actionscript-mode-map (kbd "C-c n") 'flymake-goto-next-error)


(defun as3-compile ()
  "Launch an emacs compile for the current project"
  (interactive)
  (if as3-build-file
      (let ((command (concat "ruby " as3-build-file " compile_and_show")))
	(save-some-buffers (not compilation-ask-about-save) nil)
	(setq compilation-directory (file-name-directory as3-build-file))
	(compilation-start command))))

(define-key actionscript-mode-map (kbd "C-c k") 'as3-compile)



(add-hook 'actionscript-mode-hook
          '(lambda ()
	     (flymake-as3-mode)
	     ))
