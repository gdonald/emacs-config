;;
;; me
;;
(setq user-full-name "Greg Donald"
      user-mail-address "gdonald@gmail.com")

;;
;; add melpa to package archives
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
;; add path to manually added code
;;
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;;
;; set shell to zsh
;;
(setq explicit-shell-file-name "zsh")

;;
;; re-open files to same line number as before
;;
(save-place-mode t)

;;
;; always show formatted line numbers in the left column
;;
(global-display-line-numbers-mode)
(setq-default display-line-numbers-width 2)

;;
;; always use two space indent
;;
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;;
;; never make backup files
;;
(setq make-backup-files nil)

;;
;; highlight matching parentheses immedietly
;;
(setq show-paren-mode t)
(setq show-paren-delay 0)

;;
;; highlight current line
;;
(global-hl-line-mode)

;;
;; never show startup screen
;;
(setq inhibit-startup-screen t)

;;
;; custom dashboard
;;
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Would you like to play a game?"
	dashboard-startup-banner "~/workspace/emacs-config/banner.txt"
	dashboard-center-content t
	dashboard-items '((recents  . 25)
                          (projects . 5))))

;;
;; rainbow-delimiters
;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;
;; diff-hl-mode
;;
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  (diff-hl-margin-mode))

;;
;; projectile
;;
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  :bind (("C-c p" . 'projectile-command-map)))

;;
;; doom-themes
;;
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-monokai-classic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;
;; show file size
;;
(setq size-indication-mode t)

;;
;; always follow symlinks when opening files
;;
(setq vc-follow-symlinks t)

;;
;; save history for everything
;;
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/history")

;;
;; recent files
;;
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;
;; duplicate a line
;;
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)

;;
;; comment out code
;;
(global-set-key (kbd "C-c /") 'comment-dwim)

;;
;; turn on terminal mouse and enable wheel
;;
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

;;
;; drag-stuff
;;
(use-package drag-stuff
  :ensure t
  :commands drag-stuff-global-mode
  :bind (("S-<up>" . 'drag-stuff-up)
	 ("S-<down>" . 'drag-stuff-down)
	 ("S-<left>" . 'drag-stuff-left)
	 ("S-<right>" . 'drag-stuff-right)))

;;
;; magit
;;
(use-package magit
  :ensure t)

;;
;; use company
;;
(use-package company
  :ensure t
  :commands company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config (setq company-minimum-prefix-length 1
		company-idle-delay 0.0))

;;
;; ivy
;;
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-x B" . ivy-switch-buffer-other-window))
  :commands ivy-mode
  :config (setq ivy-count-format "(%d/%d) "
		ivy-use-virtual-buffers t
		enable-recursive-minibuffers t
		search-default-mode #'char-fold-to-regexp
		ivy-re-builders-alist '((read-file-name-internal . ivy--regex-fuzzy)
					(counsel-recentf . ivy--regex-fuzzy)
					(counsel-M-x . ivy--regex-fuzzy)
					(projectile-completing-read . ivy--regex-fuzzy)
					(t . ivy--regex-plus))))

;;
;; counsel
;;
(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode)
  :bind (("C-x C-f" . 'counsel-find-file)
	 ("C-x C-r" . 'counsel-recentf)))

;;
;; lsp-mode
;;
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	lsp-headerline-arrow "→"
	lsp-warn-no-matched-clients nil
	read-process-output-max (* 1024 1024)
	gc-cons-threshold (* 100 (* 1024 1024)))
  :hook ((ruby-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;;
;; which-key
;;
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;
;; crystal-mode
;;
(use-package crystal-mode
  :ensure t)

;;
;; rust-mode
;;
(use-package rust-mode
  :ensure t
  :commands rust-mode
  :hook (rust-mode . lsp)
  )

;;
;; rspec-mode
;;
(use-package rspec-mode
  :ensure t
  :hook
  ('compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
  :custom
  (setq compilation-scroll-output t))

;;
;; add some file mode extensions
;;
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.scss.erb\\'" . scss-mode))

;;
;; multple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :bind (("C-c j" . 'mc/mark-all-dwim)
	 ("C-c l" . 'mc/edit-lines)
	 ("C-M-l" . 'er/expand-region)
	 ("C-c m" . 'mc/mark-all-like-this)
	 ("C-c ," . 'mc/mark-previous-like-this)
	 ("C-c ." . 'mc/mark-next-like-this)))

;;
;; set sbcl
;;
(setq inferior-lisp-program "sbcl")

;;
;; run yarn tests
;;
(defun yarn-run-test()
  (interactive)
  (setq file (buffer-file-name))
  
  (setq current-line (thing-at-point 'line))
  (string-match "\\(it\\|describe\\)(\\('\\|\"\\)\\(.*\\)\\('\\|\"\\)" current-line)
  (setq quot (match-string 2 current-line))
  (setq desc (match-string 3 current-line))
  
  (if (not (null desc))
      (progn
	(setq yt "*yarn test*")
	(get-buffer-create yt)
	(switch-to-buffer yt)
	(let ((default-directory (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1)))
	  ;;(setq cmd (concat "yarn testOne --no-color " file " --grep " quot desc quot))
	  (setq cmd (concat "yarn test --no-color " file " -t " quot desc quot))
	  (insert (concat "\n" cmd "\n" (make-string (length cmd) ?-) "\n\n"))
	  (start-process-shell-command "yarn" yt cmd)))))

;;
;; ielm
;;
(add-hook 'ielm-mode-hook 'eldoc-mode)
(defun g-ielm-init-history ()
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring))
(add-hook 'ielm-mode-hook 'g-ielm-init-history)
(defun g-ielm-write-history (&rest _args)
  (with-file-modes #o600
    (comint-write-input-ring)))
(advice-add 'ielm-send-input :after 'g-ielm-write-history)

;;
;; change some colors
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#808080"))))
 '(font-lock-doc-face ((t (:foreground "#808080"))))
 '(hl-line ((t (:background "#0048a3"))))
 '(region ((t (:background "#006eee")))))

(set-face-background 'default "#00000080") ;; seems to work in iTerm

(set-face-attribute 'show-paren-match nil
                    :background "#ff48a3"
                    :foreground "#ffffff")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(treemacs-all-the-icons all-the-icons use-package)))

