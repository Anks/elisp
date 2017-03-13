;; Load all custom settings

(add-to-list 'load-path "~/opt/elisp")
(require 'defaults)
(require 'ankit-init)
(require 'setup-helm)
(require 'setup-smex)
(require 'appearance)
(require 'setup-org)
(require 'mac)

;; Blog posting
(require 'posts)
(global-set-key "`" 'ong-insert-special)
(global-set-key (kbd "<C-f2>") 'god-local-mode)
(require 'programming)
