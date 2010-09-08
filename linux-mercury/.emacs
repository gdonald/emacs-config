;; aditional load-paths
(setq load-path (cons "~/.emacs.d" load-path))

;; colors
(require 'color-theme)
(setq color-theme-is-global t)
;(color-theme-euphoria)
;(color-theme-arjen)
;(color-theme-dark-laptop)
;(color-theme-gray30)
;(color-theme-clarity)
;(color-theme-hober)
(color-theme-lethe)

(add-to-list 'load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

;; cedet, required for ecb
(load-file "~/.emacs.d/cedet/common/cedet.el")
(semantic-load-enable-excessive-code-helpers)

;; emacs code browser
(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(setq ecb-auto-activate t)
(setq ecb-fix-window-size nil)
(setq ecb-layout-name "left14")
(setq ecb-source-path '("/rails/rewrite" "/rails/justapinch" "/rails/gregdonald" "~"))
(setq ecb-tip-of-the-day nil)
(setq ecb-wget-setup '("/usr/bin/wget" . other))

;; slime
(add-to-list 'load-path "~/.emacs.d/slime")
(setq inferior-lisp-program "/usr/bin/clisp")
(require 'slime)

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

;;(setq text-mode-hook (quote (turn-on-auto-fill (lambda nil (auto-fill-mode 1)))))
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
 '(default ((t (:stipple nil :background "black" :foreground "#c0c0c0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "dejavu-dejavu sans mono"))))
 '(background "blue")
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((t (:foreground "#9290ff"))))
 '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(highlight ((t (:background "CornflowerBlue"))))
 '(list-mode-item-selected ((t (:background "gold"))))
 '(makefile-space-face ((t (:background "wheat"))))
 '(mode-line ((t (:background "Navy"))))
 '(paren-match ((t (:background "darkseagreen4"))))
 '(region ((t (:background "DarkSlateBlue"))))
 '(show-paren-match ((t (:foreground "black" :background "wheat"))))
 '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))
 '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
 '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
 '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
 '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))
 '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:underline t :foreground "Blue"))))
 '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:foreground "darkGreen"))))
 '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:underline t :foreground "darkGreen"))))
 '(vhdl-speedbar-package-face ((((class color) (background dark)) (:foreground "black"))))
 '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:underline t :foreground "black"))))
 '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue")))))
