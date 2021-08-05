;; -*- lexical-binding: t; -*-

;; Load all custom settings

(add-to-list 'load-path "~/opt/elisp")

;; Clean up things before we start customising
(require 'defaults)

;; Local config (not to be committed publicly)
(if (file-exists-p "~/opt/elisp/local.el")
    (require 'local))

;; ido all the things
;;(require 'setup-ido)

;; For configuring third party packages
(require 'package-management)
(require 'appearance)
(require 'mac)

(require 'setup-abo-packages)

;; (require 'setup-smex)
(require 'setup-org)

(require 'writing)
(require 'programming)

;; All other init
(require 'misc-init)


