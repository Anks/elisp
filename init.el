;; Load all custom settings

(add-to-list 'load-path "~/opt/elisp")

;; For configuring third party packages
(require 'package-management)

(require 'defaults)
(require 'appearance)
(require 'mac)

(require 'ankit-init)
; (require 'setup-helm)
(require 'setup-smex)
(require 'setup-org)

;; Writing
(require 'writing)
(global-set-key "`" 'ong-insert-special)

(require 'programming)
