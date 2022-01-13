;;; init.el --- Emacs configuration
;;
;; @author: phdenzel
;;
;;; Commentary:
;; This file is read when Emacs starts

;;; Package archives
(require 'package)
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
(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable")
             ;; (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
             )
  (package-refresh-contents))

;;; Configs

(setq server-socket-dir (expand-file-name "~/.emacs.d/server"))

;;; Attempt to speed up startup by allocating RAM for the garbage collector
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))))
(let ((file-name-handler-alist nil)) (expand-file-name "~/.emacs.d/init.el"))

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
(org-babel-load-file (expand-file-name "~/.emacs.d/phd-emacs.org"))

;; Custom stuff in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Byte compile configs for speed-up
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;;(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
