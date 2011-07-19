;; aditional load-paths
(setq load-path (cons "~/.emacs.d" load-path))

;; colors
;(require 'color-theme)
;(setq color-theme-is-global t)
;(color-theme-tty-dark)
;(color-theme-euphoria)
;(color-theme-arjen)
;(color-theme-dark-laptop)
;(color-theme-gray30)
;(color-theme-clarity)
;(color-theme-hober)
;(color-theme-lethe)

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

;(set-background-color "ARGBBB000000")


;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;(load-file "~/.emacs.d/cedet/common/cedet.el")

;; Enable EDE (Project Management) features
;(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;(semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)


;(add-to-list 'load-path "~/.emacs.d/ecb")

;; cedet, required for ecb
;(semantic-load-enable-excessive-code-helpers)

;; emacs code browser
;(require 'ecb)
;(setq ecb-auto-activate t)
;(setq ecb-fix-window-size nil)
;(setq ecb-layout-name "left14")
;(setq ecb-source-path '("~/rails/artemis" "~/htdocs" "~"))
;(setq ecb-tip-of-the-day nil)
;(setq ecb-wget-setup '("/usr/bin/wget" . other))

;; javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; cucumber
;(add-to-list 'load-path "~/.emacs.d/plugins")
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/plugins/snippets")

;(load "cucumber-mode")
;(add-to-list 'auto-mode-alist '("\\.feature" . feature-mode))

;; php mode
(require 'php-mode)

;; rails
(setq load-path (cons "~/.emacs.d/rails" load-path))
(require 'rails)

;(setq auto-mode-alist (cons '(".erb$" . html-mode) auto-mode-alist))

;; css
(autoload 'css-mode "css-mode")
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))

;; Make all "yes or no" prompts show "y or n" instead.
;(fset 'yes-or-no-p 'y-or-n-p)

;; no cursor blinking
;(blink-cursor-mode nil)

;; show column numbers
(setq column-number-mode t)

;; always do syntax highlighting
;(setq global-font-lock-mode t)

;; use spaces to indent, not tabs
(setq indent-tabs-mode nil)

;; tab width
(setq tab-width 2)

;; no backup files
(setq make-backup-files nil)

;; save places in files
;(setq-default save-place t)
;(setq save-place-file "~/.emacs.places")

;; scrollbars to the right side
;(setq scroll-bar-mode (quote right))

(setq show-paren-mode t)
(setq size-indication-mode t)

;(setq text-mode-hook (quote (turn-on-auto-fill (lambda nil (auto-fill-mode 1)))))
;(setq transient-mark-mode t)

;; no annoying bell
;(setq ring-bell-function (lambda nil))
;(setq visible-bell t)

; truncate
(setq default-truncate-lines 1)
(setq truncate-partial-width-windows 1)


;(setq ecb-options-version "2.32")

;; custom
;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; '(truncate-lines t)
; '(blink-cursor-mode nil)
; '(column-number-mode t)
; '(ecb-layout-window-sizes (quote (("left14" (0.30935251798561153 . 0.46938775510204084) (0.30935251798561153 . 0.4897959183673469)))))
; '(ecb-options-version "2.40")
; '(fringe-mode 0 nil (fringe))
; '(global-font-lock-mode t)
; '(hscroll-margin 5)
; '(semanticdb-default-save-directory "~/.semantic")
; '(show-paren-mode t)
; '(size-indication-mode t)
; '(speedbar-frame-parameters (quote ((minibuffer) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (set-background-color "black"))))
; '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
;(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "gainsboro" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 101 :width normal :foundry "unknown" :family "Liberation Mono")))))

;(put 'upcase-region 'disabled nil)


;;(setq ri-ruby-script "~/.emacs.d/ri-emacs.rb")
;;(autoload 'ri "~/.emacs.d/ri-ruby.el" nil t)

;;(add-hook 'ruby-mode-hook (lambda ()
;;                            (local-set-key 'f1 'ri)
;;                            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
;;                            (local-set-key 'f4 'ri-ruby-show-args)))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(js2-highlight-level 3)
 '(js2-idle-timer-delay 0.1))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(put 'downcase-region 'disabled nil)

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d//slime/")
(require 'slime-autoloads)
(slime-setup)
