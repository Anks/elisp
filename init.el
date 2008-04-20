;; Load all custom settings
(add-to-list 'load-path "~/config/elisp")
(require 'ankit-init)

;; Blog posting
(require 'posts)
(global-set-key "`" 'ong-insert-special)

;; Programming
(require 'programming)

