(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" default)))
 '(package-selected-packages (quote (zenburn-theme auto-complete ein))))
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
;; (setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)
(load-theme 'zenburn t)
(setq column-number-mode t)
