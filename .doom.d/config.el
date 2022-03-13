;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Greg Donald"
      user-mail-address "gdonald@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-material)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; fonts
(setq doom-font (font-spec :family "MesloLGS NF" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "MesloLGS NF") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "MesloLGS NF" :size 11)
      doom-big-font (font-spec :family "MesloLGS NF" :size 17))

;; mostly so projectile doesn't look everywhere
(setq projectile-project-search-path '("~/workspace/"))

;; splash on the doom screen
(setq fancy-splash-image "~/.emacs.d/splash.png")

;; single keystroke undo using s-z while in insert mode
(when (timerp undo-auto-current-boundary-timer)
  (cancel-timer undo-auto-current-boundary-timer))
(fset 'undo-auto--undoable-change
  (lambda () (add-to-list 'undo-auto--undoably-changed-buffers (current-buffer))))
(fset 'undo-auto-amalgamate 'ignore)

;; duplicate a line
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "s-d") 'duplicate-line)

;; comment out code
(global-set-key (kbd "s-/") 'comment-dwim)

;; rubocop
(setq rubocop-autocorrect-on-save t)
(setq rubocop-autocorrect-command "rubocop -A --format emacs")

(custom-set-faces
  '(default ((t (:background "#000000"))))
  '(hl-line ((t (:background "#0048a3"))))
  '(region ((t (:background "#006eee"))))
  '(font-lock-comment-face ((t (:foreground "#808080"))))
  '(font-lock-doc-face ((t (:foreground "#808080")))))

;; git fringe
(custom-set-faces!
  '(fringe :background "#001f66")
  '(line-number :foreground "#cccccc" :background "#001f66")
  '(line-number-current-line :foreground "#ffffff" :background "#0048a3")
  )

;; stuff that won't auto-load
(defun my-after ()
  (interactive)
  (fringe-mode '8)
  (define-fringe-bitmap 'git-gutter-fr:added [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [255] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [1 3 7 15 31 63 127 255] nil nil '(bottom))
)
(global-set-key (kbd "<f12>") 'my-after)

;; cursor
(setq evil-normal-state-cursor '(box "#cc0000")
      evil-insert-state-cursor '(bar "#cc0000")
      evil-visual-state-cursor '(hollow "#cc0000"))

;; status bar fringe
(setq doom-modeline-bar-width 8)

;; transparency
;;(set-frame-parameter (selected-frame) 'alpha '(93 50))
;;(add-to-list 'default-frame-alist '(alpha . (93 . 50)))

;; maximize window after launch
(run-with-idle-timer 0.1 nil 'toggle-frame-maximized)
