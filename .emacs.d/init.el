;;; init.el --- Emacs configuration
;;
;; @author: phdenzel
;;
;;; Commentary:
;; This file is read when Emacs starts

;;; Package archives
(require 'package)
(setq package-enable-at-startup nil)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (defvar marmalade '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Problematic in earlier versions
(when (> emacs-major-version 23)
  ;; remove pre-installed HTTP gnu
  (setq package-archives nil)
  ;; add HTTPS archives
  ;; (add-to-list 'package-archives marmalade t)
  (add-to-list 'package-archives melpa-stable t)
  (add-to-list 'package-archives melpa t)
  (add-to-list 'package-archives gnu t))

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
;;
;;; Attempt to speed up startup by allocating RAM for the garbage collector
(setq gc-cons-threshold (* 1000 1000 1000))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1000 1000))))
(let ((file-name-handler-alist nil)) (expand-file-name "~/.emacs.d/init.el"))


(add-hook 'emacs-startup-hook
          (lambda ()
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

;;; init.el ends here
