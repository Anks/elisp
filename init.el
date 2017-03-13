;; Load all custom settings

(add-to-list 'load-path "~/opt/elisp")

(require 'package-management)
(require 'defaults)
(require 'ankit-init)
; (require 'setup-helm)
(require 'setup-smex)
(require 'appearance)
(require 'setup-org)
(require 'mac)

;; Writing
(require 'writing)
(global-set-key "`" 'ong-insert-special)

(require 'programming)
