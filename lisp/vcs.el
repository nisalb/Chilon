;;; Git interface
(use-package magit
  :init
  (setq transient-levels-file (concat user-emacs-directory ".cache/transient/levels")
	transient-values-file (concat user-emacs-directory ".cache/transient/values")
	transient-history-file (concat user-emacs-directory ".cache/transient/history"))
  :config
  (setq magit-diff-refine-hunk t
	magit-save-respository-buffer nil)
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  (dolist (fn '(magit-checkout magit-branch-and-checkout))
    (advice-add fn
		:after (lambda (&rest _)
			 (projectile-invalidate-cache nil)))))
(use-package magit-todos)

;;; Highlight vc diffs
(use-package diff-hl
  :preface
  (if (fboundp 'fringe-mode) (fringe-mode '8))
  (setq-default fringes-outside-margins t)
  :init
  (global-diff-hl-mode +1)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'vcs)
