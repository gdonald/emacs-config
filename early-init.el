;;; early-init.el --- Emacs Early Init  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst gd-gc-cons-threshold-after-startup (* 100 1024 1024))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold gd-gc-cons-threshold-after-startup)))

(setq package-quickstart t)

(provide 'early-init)
;;; early-init.el ends here
