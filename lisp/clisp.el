
(defvar inferior-lisp-program "sbcl")

(use-package sly)
(use-package sly-repl-ansi-color
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))
(use-package sly-macrostep)

(provide 'clisp)
