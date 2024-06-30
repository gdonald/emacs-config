;;; package --- Emacs Configuration
;;; Commentary:
;;; Code:

;;
;; user info
;;
(setq user-full-name "Greg Donald"
      user-mail-address "gdonald@gmail.com")

;;
;; add melpa to package archives
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;
;; initialize and refresh package list
;;
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;
;; install initial packages
;;
(let* ((package-list '(use-package)))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;
;; add path to manually added code
;;
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;;
;; set shell to zsh
;;
(setq-default explicit-shell-file-name "zsh")

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
;; always show column numbers
;;
(setq column-number-mode t)

;;
;; always use two space indent
;;
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)

;;
;; only spaces
;;
(setq-default indent-tabs-mode nil)

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
;; confirm kill
;;
(setq confirm-kill-emacs 'y-or-n-p)

;;
;; silent save
;;
(setq save-silently t)

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
        dashboard-items '((recents  . 15)
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
  (load-theme 'doom-material-dark t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
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
(setq-default savehist-file "~/.emacs.d/history")

;;
;; recent files
;;
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;
;; increase undo limit
;;
(setq undo-limit 100000000)

;;
;; duplicate a line
;;
(defun duplicate-line ()
  "Duplicate line under cursor."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank))
(global-set-key (kbd "C-c d") 'duplicate-line)

;;
;; kill current line
;;
(global-set-key (kbd "C-c k") 'kill-whole-line)

;;
;; comment out code
;;
(global-set-key (kbd "C-c /") 'comment-dwim)

;; turn on terminal mouse and enable wheel
;;
;;(xterm-mouse-mode 1)
;;(mouse-wheel-mode 1)

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
  (setq-default lsp-headerline-arrow "→"
                ;; lsp-keymap-prefix "C-c l"
                lsp-warn-no-matched-clients nil
                read-process-output-max (* 1024 1024)
                gc-cons-threshold (* 100 (* 1024 1024)))
  :hook ((ruby-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :config
  (setq lsp-restart 'ignore))

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; connect lsp and ivy
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

;; add treemacs errors
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
;; web-mode
;;
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  :custom
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;
;; scss-mode
;;
(use-package scss-mode
  :ensure t
  :commands scss-mode)
(setq-default css-indent-offset 2)

;;
;; org-mode
;;
(use-package org-roam
  :ensure t)

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
  :hook (rust-mode . lsp))

;;
;; projectile-rails
;;
(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c C-r") 'projectile-rails-command-map))

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
;; rubocop-mode
;;
(use-package rubocop
  :ensure t
  :commands rubocop
  :custom
  (add-hook 'ruby-mode-hook #'rubocop-mode))

;;
;; typescript-mode
;;
(use-package typescript-mode
  :ensure t)

;;
;; yaml-mode
;;
(use-package yaml-mode
  :ensure t
  :commands yaml-mode
  :custom
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;
;; go-mode
;;
(use-package go-mode
  :ensure t)

;;
;; raku-mode
;;
(use-package raku-mode
  :ensure t)

;;
;; multple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :bind (("C-c j" . 'mc/mark-all-dwim)
         ("C-c e" . 'mc/edit-lines)
         ("C-c m" . 'mc/mark-all-like-this)
         ("C-c ," . 'mc/mark-previous-like-this)
         ("C-c ." . 'mc/mark-next-like-this)))

;;
;; all-the-icons
;;
(use-package all-the-icons
  :ensure t)

;;
;; yasnippet
;;
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;;
;; set sbcl
;;
(setq-default inferior-lisp-program "sbcl")

;;
;; cmake-mode
;;
(use-package cmake-mode
  :ensure t)

;;
;; c language style
;;
(setq c-default-style "linux"
      c-basic-offset 2)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))

;;
;; run yarn tests
;;
(defun yarn-run-test()
  "Run a javascript test with yarn."
  (interactive)
  (let* ((file (buffer-file-name))
         (current-line (thing-at-point 'line))
         (quot nil)
         (desc nil))
    (string-match "\\(it\\|describe\\)(\\('\\|\"\\)\\(.*\\)\\('\\|\"\\)" current-line)
    (setq quot (match-string 2 current-line))
    (setq desc (match-string 3 current-line))
    (if (not (null desc))
        (progn
          (let* ((yt "*yarn test*"))
            (get-buffer-create yt)
            (switch-to-buffer yt)
            (let* ((default-directory (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1))
                   (cmd (concat "yarn test --no-color " file " -t " quot desc quot)))
              ;;(setq cmd (concat "yarn testOne --no-color " file " --grep " quot desc quot))
              (insert (concat "\n" cmd "\n" (make-string (length cmd) ?-) "\n\n"))
              (start-process-shell-command "yarn" yt cmd)))))))

;;
;; ielm
;;
(add-hook 'ielm-mode-hook 'eldoc-mode)
(defun g-ielm-init-history ()
  "Initialize saved IELM history."
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (if (fboundp 'comint-read-input-ring)
      (comint-read-input-ring)))
(add-hook 'ielm-mode-hook 'g-ielm-init-history)
(defun g-ielm-write-history (&rest _args)
  "Save IELM history."
  (with-file-modes #o600
    (if (fboundp 'comint-write-input-ring)
        (comint-write-input-ring))))
(advice-add 'ielm-send-input :after 'g-ielm-write-history)

;;
;; indent-buffer
;;
(defun indent-buffer ()
  "Re-indent current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)

;;
;; centaur-tabs
;;
(use-package centaur-tabs
  :ensure t
  :demand
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-show-count t)
  :bind
  ("C-c b" . centaur-tabs-backward)
  ("C-c f" . centaur-tabs-forward)
  ("C-c l" . centaur-tabs-move-current-tab-to-left)
  ("C-c r" . centaur-tabs-move-current-tab-to-right))

;;
;; smex
;;
(use-package smex
  :ensure t)

;;
;; flycheck
;;
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;
;; doom-modeline
;;
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;
;; imenu
;;
(menu-bar-mode 1)
(defun try-to-add-imenu ()
  "Add imenu."
  (condition-case nil (imenu-add-to-menubar "Code") (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

;;
;; marginalia-mode
;;
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;;
;; editorconfig
;;
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;
;; jsonrpc
;;
(require 'jsonrpc) ; from ~/.emacs.d/lisp/jsonrpc.el

;;
;; node path
;;
(defvar copilot-node-executable "/opt/homebrew/bin/node")

;;
;; quelpa
;;
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Install copilot from a GitHub repository using Quelpa
(quelpa '(copilot :fetcher github :repo "copilot-emacs/copilot.el"))

;; Configure copilot using use-package
(use-package copilot
  :ensure t  ; Ensure the package is installed
  :config
  ;; TODO: copilot configuration
  )
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;;
;; copilot
;;
;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "copilot-emacs/copilot.el"
;;                    :branch "main"
;;                    :files ("*.el")))

;;
;; automatically balance windows
;;
(seq-doseq (fn (list #'split-window #'delete-window))
  (advice-add fn
              :after
              #'(lambda (&rest args) (balance-windows))))

;;
;; blackjack
;;
;;(use-package blackjack
;;  :ensure t)

;;
;; change some colors
;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "PfEd" :slant normal :weight regular :height 128 :width normal))))
 '(centaur-tabs-close-selected ((t (:background "black" :foreground "brightred"))))
 '(centaur-tabs-close-unselected ((t (:foreground "white"))))
 '(centaur-tabs-default ((t nil)))
 '(centaur-tabs-selected ((t (:background "black" :foreground "brightcyan" :box nil))))
 '(centaur-tabs-unselected ((t (:inherit tab-line-tab :background "#3e3e3e" :foreground "#aaaaaa" :box nil))))
 '(font-lock-comment-face ((t (:foreground "#808080"))))
 '(font-lock-doc-face ((t (:foreground "#808080"))))
 '(hl-line ((t (:background "#0048a3"))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "yellow1" :weight light))))
 '(ivy-minibuffer-match-face-2 ((t (:inherit ivy-minibuffer-match-face-1 :background "#262626" :foreground "brightgreen" :weight semi-bold))))
 '(ivy-minibuffer-match-face-3 ((t (:inherit ivy-minibuffer-match-face-2 :foreground "brightcyan" :weight semi-bold))))
 '(ivy-minibuffer-match-face-4 ((t (:inherit ivy-minibuffer-match-face-2 :foreground "orange" :weight semi-bold))))
 '(ivy-minibuffer-match-highlight ((t nil)))
 '(lsp-face-highlight-textual ((t (:background "mediumblue" :foreground "brightcyan" :weight normal))))
 '(marginalia-documentation ((t (:foreground "cornflowerblue"))))
 '(minibuffer-prompt ((t (:foreground "brightcyan"))))
 '(region ((t (:background "#006eee")))))

(set-face-background 'default "#000000")

(set-face-attribute 'show-paren-match nil
                    :background "#ff48a3"
                    :foreground "#ffffff")

;;
;; quiet narrowing
;;
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(doom-modeline-github t)
 '(doom-modeline-minor-modes t)
 '(doom-modeline-project-detection 'projectile)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(undercover buttercup rg ag slime sly speed-type package-lint blackjack yasnippet which-key web-mode use-package smex scss-mode rust-mode rubocop rspec-mode rainbow-delimiters projectile multiple-cursors marginalia magit lsp-ui lsp-treemacs lsp-ivy flycheck drag-stuff doom-themes doom-modeline diff-hl dashboard crystal-mode counsel company centaur-tabs all-the-icons))
 '(safe-local-variable-values
   '((eval and buffer-file-name
           (not
            (eq major-mode 'package-recipe-mode))
           (or
            (require 'package-recipe-mode nil t)
            (let
                ((load-path
                  (cons "../package-build" load-path)))
              (require 'package-recipe-mode nil t)))
           (package-recipe-mode))))
 '(size-indication-mode t))

(provide '.emacs)
;;; .emacs ends here
(put 'erase-buffer 'disabled nil)
