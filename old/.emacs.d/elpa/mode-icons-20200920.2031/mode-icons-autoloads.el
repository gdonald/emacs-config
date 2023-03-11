;;; mode-icons-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mode-icons" "mode-icons.el" (0 0 0 0))
;;; Generated autoloads from mode-icons.el

(defvar mode-icons-mode nil "\
Non-nil if Mode-Icons mode is enabled.
See the `mode-icons-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `mode-icons-mode'.")

(custom-autoload 'mode-icons-mode "mode-icons" nil)

(autoload 'mode-icons-mode "mode-icons" "\
Replace the name of the current major mode with an icon.

This is a minor mode.  If called interactively, toggle the
`Mode-Icons mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='mode-icons-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "mode-icons" '("mode-icons"))

;;;***

;;;### (autoloads nil nil ("mode-icons-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mode-icons-autoloads.el ends here
