(setq user-full-name "Philipp Denzel")
(setq user-mail-address "phdenzel@gmail.com")

(require 'cl)
(setq tls-checktrust t)

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Trust-No-One test
;; (let ((bad-hosts
;;        (loop for bad
;;              in `("https://wrong.host.badssl.com/"
;;                   "https://self-signed.badssl.com/")
;;              if (condition-case e
;;                     (url-retrieve
;;                      bad (lambda (retrieved) t))
;;                   (error nil))
;;              collect bad)))
;;   (if bad-hosts
;;       (error (format "tls misconfigured; retrieved %s ok"
;;                      bad-hosts))
;;     (url-retrieve "https://badssl.com"
;;                   (lambda (retrieved) t))))

;; Load system customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Load archives - gnu, melpa, melpa-stable, marmalade
(require 'package)

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
(package-initialize)

;; Refresh archives if not in cache
(unless (and (file-exists-p "~/.emacs.d/elpa/archives/gnu")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa")
             (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable")
             ;; (file-exists-p "~/.emacs.d/elpa/archives/marmalade")
             )
  (package-refresh-contents))

;; Package install function
;; Evaluates & installs packages in list at each load, if not already installed
(defun all-pkgs-install (&rest packages)
  "Install all packages from a  list PACKAGES, if not already installed."
  (message "Running all-pkgs-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

;; Install use-package
(defun start-all-pkgs-install ()
  "Install the package 'use-package."
  (message "Initialize package install")
  ;; Start all-pkgs-install with use-package...
  (all-pkgs-install (cons 'use-package melpa))
  ;; Now everytime "use-package <p> :ensure t" is called,
  ;; the corresponding package is automatically installed,
  ;; if not already there...
  ;; This can increase the startup time of emacs substantially
  )

(condition-case nil
    (start-all-pkgs-install)
  (error
   (package-refresh-contents)
   (start-all-pkgs-install)))

;; Startup screen
(setq inhibit-startup-message t)

;; Ask for y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Reload buffer
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Mac command key remapping
(setq mac-right-command-modifier 'hyper)

;; Other global key-bindings
;; - Kill current buffer instead of any buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Correct mouse-wheel speed
;; ...and let's be honest, everyone uses it from time to time ;)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Fix macOS bugs
;; ...seem to be corrected in emacs-version > 24
(when (< emacs-major-version 25)
  (if (or type
       (eq system-type 'darwin)
       (eq system-type 'berkeley-unix))
      (setq system-name (car (split-string system-name "\\.")))))

;; Load PATH
(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat "/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path))

;; Prefer UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Tabs setting
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;; Show line numbers
;; (global-linum-mode)

;; Shut off that annoying bell
(defun phd-bell ())
(setq ring-bell-function 'phd-bell)
(setq visible-bell nil)

;; Backups
;; - collect them in .emacs.d/backups
(defvar phd-backup-directory "~/.emacs.d/backups")
;; - and if it doesn't exist, make it
(if (not (file-exists-p phd-backup-directory))
    (make-directory phd-backup-directory t))
;; - configure backup list
(setq backup-directory-alist `(("." . ,phd-backup-directory)))
;; - configure backup saving behavior
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 5
      kept-new-versions 10
      auto-save-default t
      auto-save-timeout 15
      auto-save-interval 300)

;; - configure backup deletion
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Hunspell for ispell - bundled with emacs by default since v24
(use-package ispell
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-uses-hunspell t)))

;; Clipboard settings
(setq
 x-select-enable-clipboard t
 x-select-enable-primary t
 save-interprogram-paste-before-kill t
 require-final-newline t)

;; Save settings
(if (< emacs-major-version 25)
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode t))

;; In the case file names match for different buffers
(if (< emacs-major-version)
    (require 'uniquify))
(setq uniquify-buffer-name-style 'forward)

;; No use for cl anymore
(use-package dash
  :ensure t)

(when (display-graphic-p)

  ;; no new frames when opening with Finder/Explorer
  (setq ns-pop-up-frames nil)

  ;; maximize windows
  (global-set-key (kbd "<C-s-268632077>") 'toggle-frame-fullscreen)

  ;; hide bars
  (menu-bar-mode 1) ;; if inside window, might be nice to keep
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Scroll moves cursor instead of window
  (global-set-key [wheel-up] '(lambda ()
                                (interactive)
                                (previous-line 1)))
  (global-set-key [wheel-down] '(lambda ()
                                  (interactive)
                                  (next-line 1)))

  ;; prefer utf-8 (already defined in general-behavior.el)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; Fonts - http://sourcefoundry.org/hack/ or google fonts
  (if (eq system-type 'darwin)
      ;; (set-face-attribute 'default nil :font "Hack-14")
      ;; (set-face-attribute 'default nil :font "Inconsolata-14")
      (set-face-attribute 'default nil :font "Roboto Mono-14")
      ;; (set-face-attribute 'default nil :font "Fira Mono-14")
      ;; (set-face-attribute 'default nil :font "Monaco-14")
    (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 80))

  )

(unless (display-graphic-p)
  ;; hide bars
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; UTF-8
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; activate mouse mode
  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (previous-line 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (next-line 1)))

  ;; macOS keyboard configuration
  (setq mac-option-modifier 'meta)
  (setq mac-control-modifier 'control)
  (setq mac-right-command-modifier 'super)

  ;; though COMMAND is not very useful due to global OS keybindings
  (setq mac-function-modifier 'none)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-right-option-modifier 'left)

  ;; Keyboard craziness
  ;; Meta has a prefix, by default ESC;
  ;; Very specific to my own terminal... replace with own escape sequences
  (defun phd-terminal-keys ()
    (define-key input-decode-map "\e\eOA" [M-up])
    (define-key input-decode-map "\e\eOB" [M-down])
    (define-key input-decode-map "\e[1;5A" [C-up])
    (define-key input-decode-map "\e[1;5B" [C-down])
    (define-key input-decode-map "\e\e[1;5A" [C-M-up])
    (define-key input-decode-map "\e\e[1;5B" [C-M-down])
    (define-key input-decode-map "\e\e[1;5C" [C-M-right])
    (define-key input-decode-map "\e\e[1;5D" [C-M-left])
    (define-key input-decode-map "\e\e[_" [?\C-\M- ])
    (define-key input-decode-map "\e[\177" [C-backspace]))

  (add-hook 'terminal-init-xterm-hook 'phd-terminal-keys)
  )

;; Get name of the face of the object the cursor is on
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at %d" pos))))

;; Indent all
(defun phd-iwb ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whhatitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
;;(global-set-key (kbd "C-c n") 'iwb)

;; Kill all buffers
(defun phd-nuke-all-buffers ()
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

;; Eval and replace - very useful for formulas
(defun phd-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'phd-eval-and-replace)

;; Don't load the same buffer when splitting frames
(defun phd-vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun phd-hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'phd-vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'phd-hsplit-last-buffer)

;; Go back to indentation instead of beginning of line
(global-set-key (kbd "C-a") 'back-to-indentation)

;; Efficiently move up or down
(global-set-key (kbd "<M-up>") 'backward-paragraph)
(global-set-key (kbd "<M-down>") 'forward-paragraph)

;; For orientation
;; - highlight current cursor line position
(global-hl-line-mode nil)
;; - highlight cursor line position after scroll
;; (use-package beacon
;;              :ensure t
;;              :config
;;              (beacon-mode t)
;;              ;; (setq beacon-color "#")
;;              )

;; Smart forward; built on 'expand-region ...not so smart yet
;; (use-package smart-forward
;;              :ensure t
;;              :bind
;;              (("<C-up>" . smart-up)
;;               ("<C-down>" . smart-down)
;;               ("<C-left>" . smart-backward)
;;               ("<C-right>" . smart-forward)))

;; Bookmarks; move quickly between bm's with C-c =|[|]
(use-package bm
  :ensure t
  :bind
  (("C-c =" . bm-toggle)
   ("C-c [" . bm-previous)
   ("C-c ]" . bm-next)))

;; Hydra
;; see https://github.com/abo-abo/hydra for some cool hydras
(use-package hydra
  :ensure t)

;; Ivy - deprecated
(use-package ivy
  :ensure t)

;; Counsel - common ivy-enhanced emacs commands
(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . counsel-yank-pop)))

;; Swiper - ivy-enhanced alternative for isearch
(use-package swiper
  :pin melpa-stable
  :diminish ivy-mode
  :ensure t
  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-c l" . counsel-locate))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key read-expression-map (kbd "C-r")
    #'counsel-expression-history)
  (ivy-set-actions
   'counsel-find-file
   '(("d" (lambda (x)
            (delete-file (expand-file-name x)))
      "delete")))
  (ivy-set-actions
   'ivy-switch-buffer
   '(("k" (lambda (x)
            (kill-buffer x)
            (ivy--reset-state ivy-last))
      "kill")
     ("j" ivy--switch-buffer-other-window-action
      "other window"))))

;; Ivy-hydra (ivy almost entirely replaced my helm usage as you can see)
(use-package ivy-hydra
  :ensure t)

;; Ace-jump
(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c SPC" . ace-jump-mode))

;; Avy; alternative to ace-jump-mode
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-word-1))

;; Ace-window - awesome when supercharged; see ace-window-keys.png
(use-package ace-window
  :ensure t
  :defer 1
  :bind
  ("C-x o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 2.0)
             ;; (set-face-attribute 'aw-mode-line-face nil
             ;;                     :inherit 'mode-line-buffer-id
             ;;                     :foreground "lawn green")
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        ;; aw-leading-char-style 'path
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          ;; (?u winner-undo)
          ;; (?r winner-redo)
          ))
  (when (package-installed-p 'hydra)
    ;; Window
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    ;; Frame
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))
    ;; Scrolling
    (defhydra hydra-window-scroll (:color red)
      "Scroll other window"
      ("n" scroll-other-window "scroll")
      ("p" scroll-other-window-down "scroll down"))
    ;; add to dispatch list
    (add-to-list
     'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list
     'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list
     'aw-dispatch-alist '(?\; hydra-window-frame/body) t))
  ;; (ace-window-display-mode t)
  )

(show-paren-mode t)

(electric-pair-mode t)

;; - instead of dabbrev-expand
(global-set-key (kbd "M-.") 'hippie-expand)
;; rest of the enhancements is now done with Counsel

;; Commenting
;; (defun comment-or-uncomment-region-or-line ()
;;   "Comments/Uncomments a region or the current line if no region is active"
;;   (interactive)
;;   (let (beg end)
;;     (if (region-active-p)
;;         (setq beg (region-beginning) end (region-end))
;;       (setq beg (line-beginning-position) end (line-end-position)))
;;     (comment-or-uncomment-region beg end)
;;     ;; (next-logical-line)
;;     ))
;; (defun comment-dwim-line (&optional arg)
;;    "Replacement for the comment-dwim command. If no region is selected and  current line is not blank and we are not at the end of the line, then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;;      (interactive "*P")
;;      (comment-normalize-vars)
;;      (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;;          (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;;        (comment-dwim arg)))
;; (global-set-key (kbd "M-/") 'comment-dwim-line)
(use-package comment-dwim-2
  :ensure t
  :bind
  ("M-/" . comment-dwim-2))

;; Deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure t
  :bind
  ("<C-backspace>" . hungry-delete-backward)
  ;; :config
  ;; (global-hungry-delete-mode)
  )

;; Text: spelling and wrapping
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda()
                   (flyspell-mode 1)
                   ;; (visual-line-mode 1) ;; in conflict with Swiper
                   )))

;; Autocomplete - deprecated using company instead
;; (use-package auto-complete
;;              :ensure t
;;              :init
;;              ((ac-config-default)
;;               (global-auto-complete-mode t)))

;; Company - Modern autocomplete [**Comp**lete-**any**thing]
(use-package company
  :ensure t
  :bind
  ("C-c ." . company-complete)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; - for fuzzy-complete functionality - unfortunately a bit twitchy
;; (use-package company-flx
;;              :ensure t
;;              :config
;;              (with-eval-after-load 'company
;;                (company-flx-mode t)))

;; Expand-region selection; alternatively use 'mark-sexp
(use-package expand-region
  :ensure t
  :bind
  (([?\C-\M- ] . er/expand-region)
   ))

;; Drag-stuff - simply moves entire lines
(use-package drag-stuff
  :ensure t
  :bind
  (("<C-M-up>" . drag-stuff-up)
   ("<C-M-down>" . drag-stuff-down)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("M-SPC" . set-rectangular-region-anchor)
   ("C-c ," . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<"  . mc/mark-all-like-this)
   ("<C-M-mouse-1>" . mc/add-cursor-on-click)
   ("M-m"   . hydra-mc/body)
   ("C-x m" . hydra-mc/body))
  :config
  (defhydra hydra-mc (:hint nil)
    "                                                        
    ^Up^            ^Down^        ^Miscellaneous^                
    -----------------------------------------------------------        
    [_p_]   Next    [_n_]   Next    [_l_] Edit lines  [_x_] Arrows     
    [_P_]   Skip    [_N_]   Skip    [_a_] Mark all    [_g_] Regexp     
    [_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit                         
    "
    ("l"   mc/edit-lines :exit t)
    ("a"   mc/mark-all-like-this-dwim :exit t)
    ("n"   mc/mark-next-like-this)
    ("N"   mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p"   mc/mark-previous-like-this)
    ("P"   mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("g"   mc/mark-all-in-region-regexp :exit t)
    ("r"   mc/mark-sgml-tag-pair :exit t)
    ("x"   mc/mark-more-like-this-extended)
    ("q"   nil)
    ("<mouse-1>" mc/add-cursor-on-click)
    ("<down-mouse-1>" ignore)
    ("<drag-mouse-1>" ignore))
  )

;; Global
;; - Paredit
;; (use-package paredit
;;              :ensure t
;;              :diminish paredit-mode
;;              :bind
;;              (("C-c d" . paredit-forward-down)
;;               ("C-c s" . paredit-forward-up))
;;              :config
;;              (define-key paredit-mode-map ())
;; )

;; - ensure global hook to all modes
;; (use-package paredit-everywhere
;;              :ensure t
;;              :diminish paredit-everywhere-mode
;;              :config
;;              (add-hook 'prog-mode-hook #'paredit-everywhere-mode))

;; - Error checking; might have to turn that one off again :S
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;; - Code folding
(use-package hideshow
  :ensure t
  :bind
  ("C-;" . hs-toggle-hiding)
  :config
  (setq hs-hide-comments nil)
  (setq hs-isearch-open 'x)
  (add-hook 'prog-mode-hook (lambda ()
                              (hs-minor-mode 1))))

;; - Snippets
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode 1))

;; Elisp
;; - Highlight parentheses for emacs-lisp
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode))))

;; ...and use pretty rainbow colors afterwards
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))
(global-highlight-parentheses-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'global-hightlight-parentheses-mode-hook 'rainbow-delimiters-mode)

;; - String manipulation
(use-package s
  :ensure t)

;; Clojure
;; - Snippets
(use-package clojure-snippets
  :ensure t)

;; - Cider
(use-package cider
  :ensure t
  :pin melpa-stable
  :bind
  (("M-r" . cider-namespace-refresh))
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'cider-hydra-mode)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil))

;; - Cider-Hydras
(use-package cider-hydra
  :ensure t)

;; - refactoring functions
(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda() ((clj-refactor-mode 1)
                                          (yas-minor-mode 1))))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil)
  :bind
  ("C-c '" . hydra-cljr-help-menu/body))

;; Python
(setq py-python-command "python") ;; "python3")
(setq python-shell-interpreter "python") ;; "python3")

;; - company auto-complete
(use-package company-jedi
  :ensure t
  :init
  (defun phd-python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'phd-python-mode-hook))

;; - for an awesome python dev-env
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;; - Cython
(use-package cython-mode
  :ensure t)

;; Extensive file system package
(use-package f
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :bind ("C-c m" . magit-status))

;; Magit-gitflow (not tried yet...)
;;(use-package magit-gitflow
;;           :ensure t
;;           :config
;;           (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

;; Counsel projectile - ivy-extensions to projectile
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-on))

;; Perspective
;; (use-package perspective
;;   :ensure t
;;   :config
;;   ;; Enable perspective mode
;;   (persp-mode t)
;;   (defmacro custom-persp (name &rest body)
;;     `(let ((initialize (not (gethash ,name perspectives-hash)))
;;            (current-perspective persp-curr))
;;        (persp-switch ,name)
;;        (when initialize ,@body)
;;        (setq persp-last current-perspective)))
;;   ;; Jump to last perspective
;;   (defun custom-persp-last ()
;;     (interactive)
;;     (persp-switch (persp-name persp-last)))
;;   (define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)
;;   ;; Custom mapping, e.g.
;;   (defun custom-persp/emacs ()
;;     (interactive)
;;     (custom-persp "emacs"
;;                   (find-file "~/.emacs.d/init.el")))
;;   (define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)
;;   )

(use-package perspeen
  :ensure t
  ;; :init
  ;; (setq perspeen-use-tab t)
  :config
  (perspeen-mode)
  ;; :bind
  ;; (("C-x <left>" . perspeen-tab-prev)
  ;; ("C-x <right>" . perspeen-tab-next))
)

;; (use-package web-mode
;;              :ensure t
;;              :config
;;              (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;;              (add-to-list 'auto-mode-alist '("\\.xhtml?\\'" . web-mode))
;;              (defun my-web-mode-hook ()
;;                "Hooks for Web mode."
;;                (setq web-mode-enable-auto-closing t)
;;                (setq web-mode-enable-auto-quoting t)
;;                (setq web-mode-markup-indent-offset 2))
;;              (add-hook 'web-mode-hook  'my-web-mode-hook)
;;              )

;; HTML export
(use-package htmlize
  :ensure t
  :defer t)

;; - Emmet for super fast web-dev
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'emmet-mode))

;; CSS & Co.
(use-package less-css-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  )

(use-package sass-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode))
  )

;; Markup languages
(use-package yaml-mode
  :ensure t
  :defer t)

;; Markdown (message "message" format-args)ode
(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (custom-set-variables '(markdown-command
                          "/usr/local/bin/pandoc")) 
  )

(eval-after-load "org-indent" '(diminish 'org-indent-mode))

(use-package org
  :ensure t
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
)

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)

(use-package auctex
  :defer t
  :ensure t
  :config
  (require 'tex)
  (TeX-global-PDF-mode t)
  )

(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable))

;; Which-key - helper for incomplete keystrokes
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Company doc pop-ups
(use-package company-quickhelp
  :ensure t
  :config
  (eval-after-load 'company
    '(define-key company-active-map
       (kbd "C-c h")
       #'company-quickhelp-manual-begin))
  ;; (company-quickhelp-mode 1)
  )

;; Command log mode to see what is being typed
(use-package command-log-mode
             :ensure t)

(use-package xterm-color
  :ensure t)

(require 'comint)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (setq insert-directory-program (executable-find "gls"))

;; (progn (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
;;        (setq comint-output-filter-functions (remove 'ansi-color-process-output comint-output-filter-functions)))

;; REST client
(use-package restclient
  :ensure t
  :defer t)

;; HTTP requests
(use-package request
  :ensure t
  :defer t)

;; Load the custom theme
(load-theme 'phd-dark t)

;; Transparent background
;; (set-frame-parameter (selected-frame) 'alpha '(95 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 50)))

;; Gamma adjustment
;; (setq default-frame-alist '((screen-gamma . 1.0)))
;; (setq initial-frame-alist '((screen-gamma . 1.0)))

;; Prettify symbols
(global-prettify-symbols-mode 1)

;; Load a few themes...
(use-package color-theme
  :ensure t)
(use-package base16-theme
  :ensure t)
;; (use-package zenburn-theme
;;              :ensure t)
;; (use-package spacemacs-theme
;;           :ensure t)
;; (use-package spaceline
;;           :ensure t)

;; All the pretty icons
(use-package all-the-icons
  :ensure t)

;; ...use mode-icons instead
;; (use-package mode-icons
;;           :ensure t
;;           :config
;;           (mode-icons-mode t))

(use-package powerline
  :ensure t
  :config
  ;; for terminal mode
  (unless window-system
    (defun all-the-icons-octicon (&rest _) "" "")
    (defun all-the-icons-faicon (&rest _) "" "")
    (defun all-the-icons-fileicon (&rest _) "" "")
    (defun all-the-icons-wicon (&rest _) "" "")
    (defun all-the-icons-alltheicon (&rest _) "" ""))
  ;; Vars
  (defvar phd-modeline-height 30 "modeline height")
  (defvar phd-modeline-bar-width 3 "modeline width")
  ;; Faces
  (defface phd-modeline-buffer-path
    '((t (:inherit mode-line :bold t)))
    "Face used for the dirname of the buffer path")
  (defface phd-modeline-buffer-project
    '((t (:inherit phd-modeline-buffer-path :bold nil)))
    "Face used for the filename of the modeline buffer path")
  (defface phd-modeline-buffer-modified
    '((t (:inherit highlight :background nil)))
    "Face used for the 'unsaved' symbol in the modeline")
  (defface phd-modeline-buffer-major-mode
    '((t (:inherit mode-line :bold t)))
    "Face used for the major-mode segment in the modeline")
  (defface phd-modeline-highlight
    '((t (:inherit mode-line)))
    "Face for bright segments of the modeline")
  ;; - Git/VCS segment
  (defface phd-modeline-info '((t (:inherit success)))
    "Face for info-level messages in the modeline")
  (defface phd-modeline-warning '((t (:inherit warning)))
    "Face for warnings in the modeline")
  (defface phd-modeline-error `((t (:inherit error)))
    "Face for errors in the modeline")
  ;; - Bar; TODO: check inherit colors
  (defface phd-modeline-bar
    '((t (:inherit highlight :foreground nil)))
    "The face of the lhs on the modeline of an active window")
  (defface phd-modeline-eldoc-bar
    '((t (:inherit shadow :foreground nil)))
    "The face of the lhs on the modeline when eldoc is active")
  (defface phd-modeline-inactive-bar
    '((t (:inherit mode-line-inactive)))
    "The face of lhs on the modeline of an inactive window")
  ;; Functions
  (defun phd-ml-flycheck-count (state)
    "Return flycheck information for the given error type STATE"
    (when (flycheck-has-current-errors-p state)
      (if (eq 'running flycheck-last-status-change) "?"
        (cdr-safe (assq state (flycheck-count-errors
                               flycheck-current-errors))))))
  (defun phd-make-xpm (color height width)
    "Create an XPM bitmap"
    (when window-system
      (propertize
       " " 'display
       (let ((data nil)
             (i 0))
         (setq data (make-list height (make-list width 1)))
         (pl/make-xpm "percent" color color (reverse data))))))
  ;; - Root file
  (defun phd-project-root (&optional strict-p)
    "Get the path to the root of the project"
    (let (projectile-require-project-root strict-p)
      (projectile-project-root)))
  (defun phd-buffer-path ()
    "Gets the path to the buffer"
    (if buffer-file-name
        (let* ((default-directory
                 (f-dirname buffer-file-name))
               (buffer-path (f-relative buffer-file-name
                                        (phd-project-root)))
               (max-length (truncate (* (window-body-width) 0.4))
                           ))
          (when (and buffer-path (not (equal buffer-path ".")))
            (if (> (length buffer-path) max-length)
                (let ((path (reverse (split-string
                                      buffer-path "/" t)))
                      (output ""))
                  (when (and path (equal "" (car path)))
                    (setq path (cdr path)))
                  (while (and path (<= (length output)
                                       (- max-length 4)))
                    (setq output (concat (car path) "/" output))
                    (setq path (cdr path)))
                  (when path
                    (setq output (concat "../" output)))
                  (when (string-suffix-p "/" output)
                    (setq output (substring output 0 -1)))
                  output)
              buffer-path)))
      "%b"))
  ;; - track the current window
  (defsubst active () (eq (selected-window)
                          powerline-selected-window))
  ;; - Memoize for optimization
  (pl/memoize 'phd-make-xpm)
  (pl/memoize 'face-background)
  (pl/memoize 'all-the-icons-octicon)
  ;; - modeline segments
  (defun *buffer-project ()
    ""
    (let ((face (if (active) 'phd-modeline-buffer-project)))
      (concat (all-the-icons-octicon
               "file-directory"
               :face face
               :v-adjust -0.05
               :height 1.25)
              (propertize (concat " " (abbreviate-file-name
                                       (phd-project-root)))
                          'face face))))
  ;; - Buffer status
  (defun *buffer-info ()
    ""
    (let ((all-the-icons-scale-factor 1.2)
          (modified-p (buffer-modified-p)) faces)
      (if (active)   (push 'phd-modeline-buffer-path faces))
      (if modified-p (push 'phd-modeline-buffer-modified faces))
      (concat (if buffer-read-only
                  (concat (all-the-icons-octicon
                           "lock"
                           :face 'phd-modeline-warning
                           :v-adjust -0.05)
                          " ")
                (when modified-p
                  (concat
                   (all-the-icons-faicon
                    "floppy-o"
                    :face 'phd-modeline-buffer-modified
                    :v-adjust -0.1)
                   " ")))
              (when (and buffer-file-name
                         (not (file-exists-p buffer-file-name)))
                (concat (all-the-icons-octicon
                         "circle-slash"
                         :face 'phd-modeline-error
                         :v-adjust -0.05)
                        " "))
              (propertize (phd-buffer-path)
                          'face (if faces `(:inherit ,faces))))))
  ;; - Buffer position
  (defun *buffer-position ()
    "Buffer position"
    (let ((start (window-start))
          (end (window-end))
          (pend (point-max)))
      (if (and (= start 1)
               (= end pend))
          "All"
        (cond ((= start 1) "@Top")
              ((= end pend) "@Bot")
              (t (format "@%d%%%%" (/ end 0.01 pend)))))))
  ;; - Encoding
  (defun *buffer-encoding ()
    "The encoding and eol style of the buffer"
    (concat (let ((eol-type (coding-system-eol-type
                             buffer-file-coding-system)))
              (cond ((eq eol-type 0) "LF  ")
                    ((eq eol-type 1) "CRLF  ")
                    ((eq eol-type 2) "CR  ")))
            (let* ((sys (coding-system-plist
                         buffer-file-coding-system))
                   (sys-name (plist-get sys :name))
                   (sys-cat (plist-get sys :category)))
              (cond ((memq sys-cat '(coding-category-undecided
                                     coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name sys-name)))))
            "  "))
  ;; - Modes
  (defun *major-mode ()
    "The major mode, process, environment and text-scale info."
    (propertize
     (concat (format-mode-line mode-name)
             (if (stringp mode-line-process) mode-line-process)
             (and (featurep 'face-remap)
                  (/= text-scale-mode-amount 0)
                  (format " (%+d)" text-scale-mode-amount)))
     'face (if (active) 'phd-modeline-buffer-major-mode)))
  ;; - Version control status
  (defun *vc ()
    "Displays the current branch, colored based on its state."
    (when (and vc-mode buffer-file-name)
      (let ((backend (vc-backend buffer-file-name))
            (state   (vc-state buffer-file-name))
            (face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-scale-factor 1.0)
            (all-the-icons-default-adjust -0.1))
        (concat (propertize " " 'face 'variable-pitch)
                (cond ((memq state '(edited added))
                       (if active (setq face 'phd-modeline-info))
                       (all-the-icons-octicon
                        "git-branch"
                        :face face
                        :height 1.2
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active
                           (setq face 'phd-modeline-info))
                       (all-the-icons-octicon
                        "git-merge"
                        :face face))
                      ((eq state 'needs-update)
                       (if active
                           (setq face 'phd-modeline-warning))
                       (all-the-icons-octicon
                        "arrow-down"
                        :face face))
                      ((memq state
                             '(removed conflict unregistered))
                       (if active
                           (setq face 'phd-modeline-error))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face 'mode-line))
                       (all-the-icons-octicon
                        "git-branch"
                        :face face
                        :height 1.2
                        :v-adjust -0.05)))
                " "
                (propertize
                 (substring vc-mode
                            (+ (if (eq backend 'Hg) 2 3) 2))
                 'face (if active face))
                "  "
                (propertize
                 " " 'face 'variable-pitch)))))
  ;; - Flycheck
  (defvar-local phd--flycheck-err-cache nil "")
  (defvar-local phd--flycheck-cache nil "")
  (defun *flycheck ()
    "Persistent and cached flycheck indicators in the modeline"
    (when (and (featurep 'flycheck) flycheck-mode)
      (if (or flycheck-current-errors
              (eq 'running flycheck-last-status-change))
          (or
           (and
            (or
             (eq phd--flycheck-err-cache
                 phd--flycheck-cache)
             (memq flycheck-last-status-change
                   '(running not-checked)))
            (if (eq flycheck-last-status-change 'running)
                (concat " "
                        (all-the-icons-octicon
                         "ellipsis"
                         :face 'font-lock-doc-face
                         :height 1.1
                         :v-adjust 0)
                        " ")
              phd--flycheck-cache))
           (and (setq phd--flycheck-err-cache
                      flycheck-current-errors)
                (setq phd--flycheck-cache
                      (let ((fw (phd-ml-flycheck-count
                                 'warning))
                            (fe (phd-ml-flycheck-count
                                 'error)))
                        (concat (if (or fe fw) "  ")
                                (if fe (concat
                                        (all-the-icons-octicon
                                         "circle-slash"
                                         :face 'phd-modeline-error
                                         :height 1.0
                                         :v-adjust 0)
                                        (propertize
                                         " " 'face
                                         'variable-pitch)
                                        (propertize
                                         (format "%d" fe) 'face
                                         'phd-modeline-error)
                                        " "
                                        ))
                                (if fw (concat
                                        (all-the-icons-octicon
                                         "alert"
                                         :face
                                         'phd-modeline-warning
                                         :height 0.9
                                         :v-adjust 0)
                                        (propertize
                                         " "
                                         'face
                                         'variable-pitch)
                                        (propertize
                                         (format "%d" fw)
                                         'face
                                         'phd-modeline-warning)
                                        " "
                                        ))
                                (if (or fe fw)
                                    "  "
                                  (when (active)
                                    (all-the-icons-octicon
                                     "check"
                                     :height 1.2
                                     :v-adjust -0.06))))))))
        (concat
         "  "
         (all-the-icons-octicon "check"
                                :face (if (active)
                                          'phd-modeline-info)
                                :height 1.2
                                :v-adjust -0.06)
         " "))))
  ;; - Selection
  (defun *selection-info ()
    "Information about the current selection"
    (when (and (active) evil-visual-state-p)
      (concat
       " "
       (propertize
        (let ((reg-beg (region-beginning))
              (reg-end (region-end))
              (evil (eq 'visual evil-state)))
          (let ((lines (count-lines
                        reg-beg (min (1+ reg-end) (point-max))))
                (chars (- (1+ reg-end) reg-beg))
                (cols (1+ (abs (- (evil-column reg-end)
                                  (evil-column reg-beg))))))
            (cond
             ;; rectangle selection
             ((or (bound-and-true-p rectangle-mark-mode)
                  (and evil (eq 'block evil-visual-selection)))
              (format " %dx%dB " lines (if evil cols (1- cols))))
             ;; line selection
             ((or (> lines 1) (eq 'line evil-visual-selection))
              (if (and (eq evil-state 'visual)
                       (eq evil-this-type 'line))
                  (format " %dL " lines)
                (format " %dC %dL " chars lines)))
             (t (format " %dC " (if evil chars (1- chars)))))))
        'face 'phd-modeline-highlight))))
  ;; - Macro recording
  (defun *macro-recording ()
    "Display current macro being recorded"
    (when (and (active) defining-kbd-macro)
      (let ((sep (propertize " " 'face 'phd-modeline-panel)))
        (concat sep
                (propertize (char-to-string evil-this-macro)
                            'face 'phd-modeline-panel)
                sep
                (all-the-icons-octicon "triangle-right"
                                       :face 'phd-modeline-panel
                                       :v-adjust -0.05)
                sep))))
  ;; - Media info
  (defun *media-info ()
    (cond ((eq major-mode 'image-mode)
           (let ((size (image-size
                        (image-get-display-property) :pixels)))
             (format "  %dx%d  " (car size) (cdr size))))))

  ;; modeline
  (defun phd-modeline (&optional id)
    `(:eval
      (let* (;; (meta (concat (*) (*)))
             (lhs (list (phd-make-xpm 
                         (face-background
                          (if (active)
                              'phd-modeline-bar
                            'phd-modeline-inactive-bar))
                         phd-modeline-height
                         phd-modeline-bar-width)
                        " "
                        ,(cond ((eq id 'scratch)
                                '(*buffer-project))
                               ((eq id 'media)
                                '(*media-info))
                               (t
                                '(list (*buffer-info)
                                       " L:%l/C:%c "
                                       (*buffer-position)
                                       (*flycheck)
                                       )))))
             (rhs ,(if id
                       '(list (*major-mode))
                     '(list (*major-mode)
                            " "
                            ;; (when perspeen-current-ws perspeen-modestring)
                            "  "
                            (*vc)
                            (*buffer-encoding)
                            )))
             (mid (propertize
                   " " 'display
                   `((space
                      :align-to
                      (- (+ right right-fringe right-margin)
                         ,(1+ (string-width
                               (format-mode-line rhs)))))))))
        (list lhs mid rhs))))
  ;; Activate/set as default
  (setq-default mode-line-format (phd-modeline))
  ;; (add-hook image-mode-hook
  ;;            (setq mode-line-format (phd-modeline 'media)))
  )