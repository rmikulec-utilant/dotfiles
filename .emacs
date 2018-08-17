; quiet nagging warnings
(setq warning-minimum-level :emergency)

; list the packages you want
(setq package-list '(package auto-complete zenburn-theme magit))


(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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

(when (fboundp 'winner-mode)
  (winner-mode 1))

;; add magit for git
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
