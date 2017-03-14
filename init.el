;; Load all custom settings

(add-to-list 'load-path "~/opt/elisp")

;; Clean up things before we start customising
(require 'defaults)
;; ido all the things
(require 'setup-ido)

;; For configuring third party packages
(require 'package-management)


(require 'appearance)
(require 'mac)

;; All other init
(require 'ankit-init)

; (require 'setup-helm)
(require 'setup-smex)
(require 'setup-org)

;; Writing
(require 'writing)

(require 'programming)
