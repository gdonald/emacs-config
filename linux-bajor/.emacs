;; aditional load-paths
(setq load-path (cons "~/.emacs.d" load-path))

;; colors
(require 'color-theme)
(setq color-theme-is-global t)
;(color-theme-tty-dark)
;(color-theme-euphoria)
;(color-theme-arjen)
;(color-theme-dark-laptop)
;(color-theme-gray30)
;(color-theme-clarity)
(color-theme-hober)
;(color-theme-lethe)

;; cedet, required for ecb
;(semantic-load-enable-excessive-code-helpers)

;; emacs code browser
;(require 'ecb)
;(setq ecb-auto-activate t)
;(setq ecb-fix-window-size nil)
;(setq ecb-layout-name "left14")
;(setq ecb-source-path '("~/rails/artemis" "~/rails/artemis-dump" "~/htdocs" "~" "~/rails/asklepion"))
;(setq ecb-tip-of-the-day nil)
;(setq ecb-wget-setup '("/usr/bin/wget" . other))

;; slime
;(setq inferior-lisp-program "/usr/bin/clisp")
;(require 'slime)

;; php mode
(require 'php-mode)

;; rails
(setq load-path (cons "~/.emacs.d/rails" load-path))
(require 'rails)

;; css
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Make all "yes or no" prompts show "y or n" instead.
(fset 'yes-or-no-p 'y-or-n-p)

;; no cursor blinking
(blink-cursor-mode nil)

;; show column numbers
(setq column-number-mode t)

;; always do syntax highlighting
(setq global-font-lock-mode t)

;; use spaces to indent, not tabs
(setq indent-tabs-mode nil)

;; tab width
(setq tab-width 2)

;; no backup files
(setq make-backup-files nil)

;; save places in files
(setq-default save-place t)
(setq save-place-file "~/.emacs.places")

;; scrollbars to the right side
(setq scroll-bar-mode (quote right))

(setq show-paren-mode t)
(setq size-indication-mode t)

(setq text-mode-hook (quote (turn-on-auto-fill (lambda nil (auto-fill-mode 1)))))
(setq transient-mark-mode t)

;; no annoying bell
(setq ring-bell-function (lambda nil))
;(setq visible-bell t)

(setq ecb-options-version "2.32")

;; custom
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes (quote (("left14" (0.30935251798561153 . 0.46938775510204084) (0.30935251798561153 . 0.4897959183673469)))))
 '(ecb-options-version "2.32")
 '(global-font-lock-mode t)
 '(semanticdb-default-save-directory "~/.semantic")
 '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black")))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)

(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

;;(setq ri-ruby-script "~/.emacs.d/ri-emacs.rb")
;;(autoload 'ri "~/.emacs.d/ri-ruby.el" nil t)

;;(add-hook 'ruby-mode-hook (lambda ()
;;                            (local-set-key 'f1 'ri)
;;                            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                            (local-set-key 'f4 'ri-ruby-show-args)))
