;;; init.el --- Emacs configuration
;;
;; @author: phdenzel
;;
;;; Commentary:
;; This file is read when Emacs starts

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Code:

;; Attempts to speed up startup
(setq gc-cons-threshold 10000000)
(let ((file-name-handler-alist nil)) "~/.emacs.d/init.el")

(require 'org)

(org-babel-load-file (expand-file-name "~/.emacs.d/phd-emacs.org"))
;;; init.el ends here
