;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;;
(setq explicit-shell-file-name "zsh")

;;
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes"))
(load-theme 'billw t t)
(enable-theme 'billw)

;;
(require 'powerline)
(powerline-center-theme)

;;
(global-linum-mode t)
(setq linum-format "%4d \u2502 ")
(setq column-number-mode t)

;;
(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)

;;
(setq make-backup-files nil)

;;
(setq show-paren-mode t)

;;
(setq size-indication-mode t)

;;
(setq inhibit-startup-screen t)

;;
(show-paren-mode 1)

;;
(setq vc-follow-symlinks t)

;;
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/history")

;;
(company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

;;
(require 'lsp-mode)
(add-hook 'prog-mode-hook 'lsp-deferred)
(setq lsp-warn-no-matched-clients nil)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold (* 100 (* 1024 1024)))

;;
(require 'rspec-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter-and-focus)
(setq compilation-scroll-output t)

;;
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.scss.erb\\'" . scss-mode))

;;
(setq inferior-lisp-program "sbcl")

;;
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-d") 'duplicate-line)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(spaceline vterm mode-icons powerline-evil powerline ## lsp-latex yasnippet dap-mode helm-lsp lsp-treemacs company lsp-ui lsp-mode selectrum magit inf-ruby rspec-mode slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
