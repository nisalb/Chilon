;; Chilon Emacs
;; "Less is More" -- Chilon of Sparta

;; add <CHILON>/lisp to load path
(push (concat user-emacs-directory "lisp/") load-path)

;; General emacs configurations
(setq straight-repository-branch "develop")

;; UI preferences
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; Separate dirs for backups and auto saves
(setq require-final-newline t
      create-lockfiles nil
      make-backup-files nil
      ; backup-by-copying t ; uncomment this when enabling backup files
      auto-save-default t
      auto-save-list-file-prefix (concat user-emacs-directory ".cache/saves/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ;; Prefix tramp autosaves to prevent conflicts with local ones
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Setup straight and use-package for package management
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; And then configure straight to use use-package by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Stimmung is another minimal theme, with bare minimum of highlights
(use-package stimmung-themes
  :config
  (setq stimmung-themes-comment 'foreground) 
  (stimmung-themes-load-light))

;; Next I need the support for ligatures.
(use-package ligature
  :config
  ;; Enable ligatures in all programming modes
  (ligature-set-ligatures 'prog-mode
			  '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
			    ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t)
  ;; But I don't need them in C. Because, I want to feel C as a low level language
  (dolist (mode '(c-mode-common-hook))
    (add-hook mode
	      (lambda ()
		(ligature-mode -1)))))

;; For engaging experience with Emacs
(use-package which-key
  :config
  (which-key-mode))

;; This is for vertico. For vertico to sort by history position
(use-package savehist
  :init
  (savehist-mode))

;; Enahnce the completion UI
(require 'completion)

;; Now the basic UI requirements are complete.

;; Managing projects with projectile
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

;; Terminal integration for Emacs with vterm
(use-package vterm
  :bind ("C-c t" ("Open terminal by side" . vterm-other-window)))

;; Treat undos as a tree, rather than a linear sequence of changes
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(concat user-emacs-directory ".cache/undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-auto-save-history t
	undo-tree-enable-undo-in-region t
	ndo-limit 800000 
        undo-strong-limit 12000000
        undo-outer-limit 128000000)
  (advice-add 'undo-tree-make-history-file-name
	      :filter-return (lambda (file)
			       (cond ((executable-find "zstd")
				      (concat file ".zst"))
				     ((executable-find "gzip")
				      (concat file ".gz"))
				     (t (concat file "")))))
  (advice-add 'undo-list-transfer-to-tree	
	      :before (lambda (&rest _)
			(dolist (item buffer-undo-list)
			  (and (consp item)
			       (stringp (car item))
			       (setcar item (substring-no-properties (car item))))))))

;; Vi mode is better. I think...
(use-package evil
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t  ; column range for ex commands
        ;; more vim-like behavior
        evil-symbol-word-search t
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t
        evil-undo-system 'undo-tree
	evil-default-state 'emacs)
  :init
  (evil-mode))

;; Easy window select
(use-package ace-window
  :init
  (global-set-key [remap other-window] #'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
	aw-scope 'frame
        aw-background t))

;; Activate org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)
   (emacs-lisp . t)
   (lisp . t)))

;; Paredit mode for Lisps
(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode clojure-mode lisp-interaction-mode scheme-mode) . paredit-mode))

;; VCS Integration
(require 'vcs)

;;; Ruby
(require 'ruby)

;; Common Lisp
(require 'clisp)

;; Yaml
(use-package yaml-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Lambda Mono" :foundry "nil" :slant normal :weight regular :height 120 :width normal)))))
