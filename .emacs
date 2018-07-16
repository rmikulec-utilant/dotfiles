; quiet nagging warnings
(setq warning-minimum-level :emergency)

; list the packages you want
(setq package-list '(package auto-complete zenburn-theme))

; list the repositories containing them
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'package) ;; You might already have this line

(require 'auto-complete)
(global-auto-complete-mode t)

(load-theme 'zenburn t)

(setq column-number-mode t)

(setq-default show-trailing-whitespace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
