;;; custom.el --- Customize settings  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-ts-indent-offset 2 nil nil "Customized with use-package c-ts-mode")
 '(package-selected-packages
   '(adoc-mode agent-shell blackjack buttercup centaur-tabs cider
               clojure-ts-mode consult corfu crystal-mode dashboard
               diff-hl doom-modeline doom-themes drag-stuff embark
               embark-consult gdscript-mode haml-mode kotlin-ts-mode
               magit marginalia multiple-cursors nerd-icons
               nerd-icons-completion orderless org-roam
               projectile-rails rainbow-delimiters raku-mode rbenv
               robe rspec-mode rubocop scss-mode sly smalltalk-mode
               swift-mode tempel tempel-collection terraform-mode
               undercover vertico web-mode zig-ts-mode))
 '(safe-local-variable-values
   '((eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode)))))
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
 '(eglot-highlight-symbol-face ((t (:background "mediumblue" :foreground "brightcyan" :weight normal))))
 '(font-lock-comment-face ((t (:foreground "#808080"))))
 '(font-lock-doc-face ((t (:foreground "#808080"))))
 '(marginalia-documentation ((t (:foreground "cornflowerblue"))))
 '(minibuffer-prompt ((t (:foreground "brightcyan"))))
 '(orderless-match-face-0 ((t (:background "#262626" :foreground "brightgreen" :weight semi-bold))))
 '(orderless-match-face-1 ((t (:background "#262626" :foreground "brightcyan" :weight semi-bold))))
 '(orderless-match-face-2 ((t (:background "#262626" :foreground "orange" :weight semi-bold))))
 '(orderless-match-face-3 ((t (:background "#262626" :foreground "yellow1" :weight semi-bold))))
 '(org-block ((t (:background "#1c1c1c" :extend t))))
 '(org-block-begin-line ((t (:background "#1c1c1c"))))
 '(region ((t (:background "#006eee")))))
