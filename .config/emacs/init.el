;;; init.el --- Emacs configuration
;;
;; @author: phdenzel
;;
;;; Commentary:
;; This file is read when Emacs starts

;;; Package archives
(require 'package)
;;; Code:
(setq package-enable-at-startup nil)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defvar archive-gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar archive-melpa '("melpa" . "https://melpa.org/packages/"))
(defvar archive-melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (defvar marmalade '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Problematic in earlier versions
(when (> emacs-major-version 23)
  ;; remove pre-installed HTTP gnu
  (setq package-archives nil)
  ;; add HTTPS archives
  ;; (add-to-list 'package-archives marmalade t)
  (add-to-list 'package-archives archive-melpa-stable t)
  (add-to-list 'package-archives archive-melpa t)
  (add-to-list 'package-archives archive-gnu t))

;;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Refresh archives if not in cache
(unless (and (file-exists-p "~/.config/emacs/elpa/archives/gnu")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa")
             (file-exists-p "~/.config/emacs/elpa/archives/melpa-stable")
             ;; (file-exists-p "~/.config/emacs/elpa/archives/marmalade")
             )
  (package-refresh-contents))

;;; Configs

(setq server-socket-dir (expand-file-name "~/.config/emacs/server"))

;;; Attempt to speed up startup by allocating RAM for the garbage collector
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold to 16 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 24))))
(let ((file-name-handler-alist nil)) (expand-file-name "~/.config/emacs/init.el"))

(add-hook 'emacs-startup-hook
          (lambda()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Load org configs
(require 'org)
(org-babel-load-file (expand-file-name "~/.config/emacs/phd-emacs.org"))

;; Custom stuff in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Byte compile configs for speed-up
(byte-recompile-directory (expand-file-name "~/.config/emacs") 0)

;;(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
