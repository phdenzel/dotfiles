;;; init.el --- Emacs configuration
;;
;; @author: phdenzel
;;
;;; Commentary:
;; This file is read when Emacs starts

;;; Code:

;;; General configs
(defvar gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defvar server-socket-dir (expand-file-name "~/.config/emacs/server"))


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


;;; straight.el
;; Disable package.el
(setq package-enable-at-startup nil)
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;;; Load org configs
(straight-use-package 'org)
(org-babel-load-file (expand-file-name "~/.config/emacs/phd-emacs.org"))


;; Custom stuff in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;;; init.el ends here
