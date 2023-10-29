;; Packages and configurations required for programming in Ruby

;; ruby-mode is built-in, so it is not required
(use-package ruby-mode
  :config
  (subword-mode +1))

;; for YARD tags in documentations
(use-package yard-mode
  :hook ruby-mode)

;; to start a repl
(use-package inf-ruby)

(provide 'ruby)
