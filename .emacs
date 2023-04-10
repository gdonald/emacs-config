;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
(setq load-path (cons "~/.emacs.d/lisp" load-path))

;;
(setq explicit-shell-file-name "zsh")

;;
(save-place-mode t)

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
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
;; fuzzy filenames
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

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
(global-set-key (kbd "C-c j") 'mc/mark-all-dwim)
(global-set-key (kbd "C-c l") 'mc/edit-lines)
(global-set-key (kbd "C-M-l") 'er/expand-region)
(global-set-key (kbd "C-c /") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c .") 'mc/mark-next-like-this)

;;
(global-set-key (kbd "C-c <up>") 'move-text-up)
(global-set-key (kbd "C-c <down>") 'move-text-down)

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
 '(column-number-mode t)
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("eb7cd622a0916358a6ef6305e661c6abfad4decb4a7c12e73d6df871b8a195f8" default))
 '(package-selected-packages
   '(vterm counsel ivy modus-themes company use-package diff-hl move-text expand-region multiple-cursors mode-icons powerline ## lsp-latex yasnippet dap-mode helm-lsp lsp-treemacs lsp-ui lsp-mode selectrum magit inf-ruby rspec-mode slime))
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :extend nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "MesloLGS NF")))))
