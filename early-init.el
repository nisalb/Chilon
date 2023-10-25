;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Redirect eln-cache
(setq native-comp-eln-load-path (cdr native-comp-eln-load-path))
(push (expand-file-name ".cache/eln-cache/" user-emacs-directory)
      native-comp-eln-load-path)
