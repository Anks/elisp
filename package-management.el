;;; Inspired from Mike Zamansky's config

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; Enable 'try' to test out a bunch of different packages safely
(use-package try
  :ensure t)

(use-package hydra
  :ensure t)

(provide 'package-management)
