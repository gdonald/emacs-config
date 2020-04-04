(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "Your version of Emacs does not support SSL connections"))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(setq load-path (cons "~/.emacs.d/lisp" load-path))

(setq explicit-shell-file-name "/usr/local/bin/zsh")

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.scss.erb\\'" . scss-mode))

(global-linum-mode t)
(setq linum-format "%4d \u2502 ")
(setq column-number-mode t)

(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(setq make-backup-files nil)
(setq show-paren-mode t)
(setq size-indication-mode t)

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/history")

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
	  (setq cmd (concat "yarn testOne --no-color " file " --grep " quot desc quot))
	  (insert (concat "\n" cmd "\n" (make-string (length cmd) ?-) "\n\n"))
	  (start-process-shell-command "yarn" yt cmd)))))

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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'rspec-mode)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-inf-ruby))
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

(setq vc-follow-symlinks t)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;(unless window-system
;  (require 'mouse)
;  (xterm-mouse-mode t)
;  (defun track-mouse (e)) 
;  (setq mouse-sel-mode t)
;  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (billw)))
 '(custom-safe-themes
   (quote
    ("5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "9d9b2cf2ced850aad6eda58e247cf66da2912e0722302aaa4894274e0ea9f894" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "be5b03913a1aaa3709d731e1fcfd4f162db6ca512df9196c8d4693538fa50b86" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" default)))
 '(package-selected-packages
   (quote
    (scss-mode slime company ag company-inf-ruby color-theme-modern rspec-mode projectile flycheck-perl6 flymake-haml flymake-json flymake-ruby flymake-sass flymake-yaml haml-mode perl6-mode smartparens web-mode company-emoji emojify projectile-rails)))
 '(recentf-auto-cleanup (quote never))
 '(recentf-mode t)
 '(safe-local-variable-values
   (quote
    ((projectile-rails-verify-root-file . "config/routes")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
