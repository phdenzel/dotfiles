;;; phd-dark.el ---  phdenzel's ark theme for Emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright 2019-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Author: phdenzel
;; Maintainer: phdenzel
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/phdenzel/dotfiles/tree/master/.config/emacs

;;; Commentary:
;;; Should also work in Terminal.

;;; Code:
(deftheme phd-ark)

;;; Configuration options

(defgroup phd-ark nil
  "Options for the phd-ark theme.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom phd-ark-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boolean
  :group 'phd-ark)

(defcustom phd-ark-height-title-1 1.0
  "Header 1 font size."
  :type 'number
  :group 'phd-ark)

(defcustom phd-ark-height-title-2 1.0
  "Header 2 font size."
  :type 'number
  :group 'phd-ark)

(defcustom phd-ark-height-title-3 1.0
  "Header 3 font size."
  :type 'number
  :group 'phd-ark)

(defcustom phd-ark-height-doc-title 1.0
  "Documentation Title font size."
  :type 'number
  :group 'phd-ark)

(defcustom phd-ark-highlight-matches nil
  "Use background color to make highlighted matches more visible."
  :type 'boolean
  :group 'phd-ark)

(defcustom phd-ark-italic-comments nil
  "Use :slant italic for comments."
  :type 'boolean
  :group 'phd-ark)

(defcustom phd-ark-italic-blockquotes t
  "Use :slant italic for blockquotes in markdown and org."
  :type 'boolean
  :group 'phd-ark)

(defcustom phd-ark-italic-variables nil
  "Use :slant italic for variables."
  :type 'boolean
  :group 'phd-ark)

(defcustom phd-ark-gui-font "Fira Mono-13"
  "The font to use for the phd-ark theme in graphical mode."
  :type 'string
  :group 'phd-ark)

(defcustom phd-ark-terminal-font "Fira Mono-12"
  "The font to use for the phd-ark theme in non-graphical mode."
  :type 'string
  :group 'phd-ark)

(defcustom phd-ark-flavor 'iridis
  "The flavor to use for the phd-ark theme.
Must be one of `iridis`, `dark`, or `light`."
  :type '(choice (const :tag "Iridis" iridis)
                 (const :tag "Dark" dark)
                 (const :tag "Light" light))
  :group 'phd-ark)

;;; Theme flavor color palettes
(defcustom phd-ark-iridis-colors '((magenta   . "#FF0087")
                                   (pink      . "#E83A82")
                                   (ruby      . "#D7005F")
                                   (crimson   . "#D70000")
                                   (red       . "#FF6D6B")
                                   (tiger     . "#FF5F00")
                                   (orange    . "#F69927")
                                   (sand      . "#FDB760")
                                   (yellow    . "#FFD787")
                                   (green     . "#44BC84")
                                   (grass     . "#3DAA77")
                                   (emerald   . "#00AF5F")
                                   (viridis   . "#00AF87")
                                   (teal      . "#4DB5BD")
                                   (ocean     . "#1F5582")
                                   (cyan      . "#46D9FF")
                                   (blue      . "#5F8AF7")
                                   (indigo    . "#5F5FFF")
                                   (amethyst  . "#3723B7")
                                   (purple    . "#8787FF")
                                   (violet    . "#A9A1E1")
                                   (white     . "#F6F9FE")
                                   (subtext0  . "#EDF3FE")
                                   (subtext1  . "#DDE3EE")
                                   (text      . "#BDC3CE")
                                   (overlay2  . "#8A8A8A")
                                   (overlay1  . "#464858")
                                   (overlay0  . "#5B6268")
                                   (surface2  . "#515C66")
                                   (surface1  . "#404850")
                                   (surface0  . "#3A3C3F")
                                   (base      . "#303033")
                                   (mantle    . "#2B2B2F")
                                   (crust     . "#181E26"))
  "Colors used for phd-ark iridis."
  :tag "Iridis Colors"
  :options '(magenta pink ruby crimson red tiger orange sand yellow green grass emerald viridis teal ocean cyan blue indigo amethyst purple violet white subtext0 subtext1 text overlay2 overlay1 overlay0 surface2 surface1 surface0 base mantle crust)
  :type '(alist :key-type symbol :value-type string)
  :group 'phd-ark)

(defcustom phd-ark-dark-colors '((magenta   . "#FF0087")
                                 (pink      . "#E83A82")
                                 (ruby      . "#D7005F")
                                 (crimson   . "#D70000")
                                 (red       . "#FF6D6B")
                                 (tiger     . "#FF5F00")
                                 (orange    . "#F69927")
                                 (sand      . "#FDB760")
                                 (yellow    . "#FFD787")
                                 (green     . "#44BC84")
                                 (grass     . "#3DAA77")
                                 (emerald   . "#00AF5F")
                                 (viridis   . "#00AF87")
                                 (teal      . "#4DB5BD")
                                 (ocean     . "#1F5582")
                                 (cyan      . "#46D9FF")
                                 (blue      . "#5F8AF7")
                                 (indigo    . "#5F5FFF")
                                 (amethyst  . "#3723B7")
                                 (purple    . "#8787FF")
                                 (violet    . "#A9A1E1")
                                 (white     . "#F6F9FE")
                                 (subtext0  . "#EDF3FE")
                                 (subtext1  . "#DDE3EE")
                                 (text      . "#BDC3CE")
                                 (overlay2  . "#6D6D6D")
                                 (overlay1  . "#353642")
                                 (overlay0  . "#494E53")
                                 (surface2  . "#404850")
                                 (surface1  . "#2E353A")
                                 (surface0  . "#27292B")
                                 (base      . "#1D1D1F")
                                 (mantle    . "#18181B")
                                 (crust     . "#090B0E"))
  "Colors used for phd-ark dark."
  :tag "Dark Colors"
  :options '(magenta pink ruby crimson red tiger orange sand yellow green grass emerald viridis teal ocean cyan blue indigo amethyst purple violet white subtext0 subtext1 text overlay2 overlay1 overlay0 surface2 surface1 surface0 base mantle crust)
  :type '(alist :key-type symbol :value-type string)
  :group 'phd-ark)

(defcustom phd-ark-light-colors '((magenta   . "#FF0087")
                                  (pink      . "#E83A82")
                                  (ruby      . "#D7005F")
                                  (crimson   . "#D70000")
                                  (red       . "#FF6D6B")
                                  (tiger     . "#FF5F00")
                                  (orange    . "#F69927")
                                  (sand      . "#FDB760")
                                  (yellow    . "#FFD787")
                                  (green     . "#44BC84")
                                  (grass     . "#3DAA77")
                                  (emerald   . "#00AF5F")
                                  (viridis   . "#00AF87")
                                  (teal      . "#4DB5BD")
                                  (ocean     . "#1F5582")
                                  (cyan      . "#46D9FF")
                                  (blue      . "#5F8AF7")
                                  (indigo    . "#5F5FFF")
                                  (amethyst  . "#3723B7")
                                  (purple    . "#8787FF")
                                  (violet    . "#A9A1E1")
                                  (white     . "#F6F9FE")
                                  (subtext0  . "#303033")
                                  (subtext1  . "#2B2B2F")
                                  (text      . "#181E26")
                                  (overlay2  . "#5B6268")
                                  (overlay1  . "#464858")
                                  (overlay0  . "#8A8A8A")
                                  (surface2  . "#3A3C3F")
                                  (surface1  . "#404850")
                                  (surface0  . "#515C66")
                                  (base      . "#EDF3FE")
                                  (mantle    . "#DDE3EE")
                                  (crust     . "#BDC3CE"))
  "Colors used for phd-ark light."
  :tag "Light Colors"
  :options '(magenta pink ruby crimson red tiger orange sand yellow green grass emerald viridis teal ocean cyan blue indigo amethyst purple violet white subtext0 subtext1 text overlay2 overlay1 overlay0 surface2 surface1 surface0 base mantle crust)
  :type '(alist :key-type symbol :value-type string)
  :group 'phd-ark)


;;; Internal functions
(defun phd-ark-quantize-color (color)
  "Quantize COLOR to a 256 color palette."
  (let ((i 1)
        (str "#"))
    (while (<= i 5)
      (setq str (concat
                 str
                 (format
                  "%02x"
                  (* (round
                      (/
                       (string-to-number (substring color i (+ i 2)) 16)
                       17))
                     17)))
            )
      (setq i (+ i 2)))
    str))

;; Color operations
(let* ((hex-to-rgb
        (lambda (color)
          (mapcar (lambda (i)
                    (string-to-number (substring color i (+ i 2)) 16))
                  '(1 3 5))))
       (rgb-to-hex
        (lambda (r g b)
          (format "#%02x%02x%02x" r g b)))
       (rnd
        (lambda (n)
          (round (+ .5 n)))))

  (defun phd-ark-lighten (color value)
    "Lighten COLOR by VALUE%."
    (let* ((factor (/ value 100.0)))
      (apply rgb-to-hex
             (mapcar
              (lambda (v) (funcall rnd (min 255 (+ (* (- 255 v) factor) v))))
              (funcall hex-to-rgb color)))))

  (defun phd-ark-darken (color value)
    "Darken COLOR by VALUE%."
    (let* ((factor (/ value 100.0)))
      (apply rgb-to-hex
             (mapcar (lambda (v) (floor (* (- 1 factor) v)))
                     (funcall hex-to-rgb color))))))


;;; User functions
(defun phd-ark-reload ()
  "Reload the phd-ark theme, useful after `phd-ark-set-color`."
  (interactive)
  (disable-theme 'phd-ark)
  (load-theme 'phd-ark t))

(defun phd-ark-set-color (color value &optional flavor)
  "Set the COLOR of FLAVOR or the current flavor to VALUE."
  (interactive "SChange color: \nsSet %s to: ")
  (setcdr
   (assoc color (symbol-value
                 (intern-soft
                  (concat "phd-ark-"
                          (symbol-name (or flavor phd-ark-flavor))
                          "-colors")))) value))

(defun phd-ark-get-color (color &optional flavor)
  "Get the COLOR of FLAVOR or the current flavor."
  (interactive "SThe color to get: ")
  (alist-get color
             (symbol-value
              (intern-soft
               (concat "phd-ark-"
                       (symbol-name (or flavor phd-ark-flavor))
                       "-colors")))))


;;;; Theme definition:
(let ((colors
       '((undef        "#ff00ff" "#ff00ff")
         (phd-magenta  (phd-ark-get-color 'magenta)
                       (phd-ark-quantize-color (phd-ark-get-color 'magenta)))
         (phd-pink     (phd-ark-get-color 'pink)
                       (phd-ark-quantize-color (phd-ark-get-color 'pink)))
         (phd-ruby     (phd-ark-get-color 'ruby)
                       (phd-ark-quantize-color (phd-ark-get-color 'ruby)))
         (phd-crimson  (phd-ark-get-color 'crimson)
                       (phd-ark-quantize-color (phd-ark-get-color 'crimson)))
         (phd-red      (phd-ark-get-color 'red)
                       (phd-ark-quantize-color (phd-ark-get-color 'red)))
         (phd-tiger    (phd-ark-get-color 'tiger)
                       (phd-ark-quantize-color (phd-ark-get-color 'tiger)))
         (phd-orange   (phd-ark-get-color 'orange)
                       (phd-ark-quantize-color (phd-ark-get-color 'orange)))
         (phd-sand     (phd-ark-get-color 'sand)
                       (phd-ark-quantize-color (phd-ark-get-color 'sand)))
         (phd-yellow   (phd-ark-get-color 'yellow)
                       (phd-ark-quantize-color (phd-ark-get-color 'yellow)))
         (phd-green    (phd-ark-get-color 'green)
                       (phd-ark-quantize-color (phd-ark-get-color 'green)))
         (phd-grass    (phd-ark-get-color 'grass)
                       (phd-ark-quantize-color (phd-ark-get-color 'grass)))
         (phd-emerald  (phd-ark-get-color 'emerald)
                       (phd-ark-quantize-color (phd-ark-get-color 'emerald)))
         (phd-viridis  (phd-ark-get-color 'viridis)
                       (phd-ark-quantize-color (phd-ark-get-color 'viridis)))
         (phd-teal     (phd-ark-get-color 'teal)
                       (phd-ark-quantize-color (phd-ark-get-color 'teal)))
         (phd-ocean    (phd-ark-get-color 'ocean)
                       (phd-ark-quantize-color (phd-ark-get-color 'ocean)))
         (phd-cyan     (phd-ark-get-color 'cyan)
                       (phd-ark-quantize-color (phd-ark-get-color 'cyan)))
         (phd-blue     (phd-ark-get-color 'blue)
                       (phd-ark-quantize-color (phd-ark-get-color 'blue)))
         (phd-indigo   (phd-ark-get-color 'indigo)
                       (phd-ark-quantize-color (phd-ark-get-color 'indigo)))
         (phd-amethyst (phd-ark-get-color 'amethyst)
                       (phd-ark-quantize-color (phd-ark-get-color 'amethyst)))
         (phd-purple   (phd-ark-get-color 'purple)
                       (phd-ark-quantize-color (phd-ark-get-color 'purple)))
         (phd-violet   (phd-ark-get-color 'violet)
                       (phd-ark-quantize-color (phd-ark-get-color 'violet)))
         (phd-white    (phd-ark-get-color 'white)
                       (phd-ark-quantize-color (phd-ark-get-color 'white)))
         (phd-subtext0 (phd-ark-get-color 'subtext0)
                       (phd-ark-quantize-color (phd-ark-get-color 'subtext0)))
         (phd-subtext1 (phd-ark-get-color 'subtext1)
                       (phd-ark-quantize-color (phd-ark-get-color 'subtext1)))
         (phd-text     (phd-ark-get-color 'text)
                       (phd-ark-quantize-color (phd-ark-get-color 'text)))
         (phd-overlay2 (phd-ark-get-color 'overlay2)
                       (phd-ark-quantize-color (phd-ark-get-color 'overlay2)))
         (phd-overlay1 (phd-ark-get-color 'overlay1)
                       (phd-ark-quantize-color (phd-ark-get-color 'overlay1)))
         (phd-overlay0 (phd-ark-get-color 'overlay0)
                       (phd-ark-quantize-color (phd-ark-get-color 'overlay0)))
         (phd-surface2 (phd-ark-get-color 'surface2)
                       (phd-ark-quantize-color (phd-ark-get-color 'surface2)))
         (phd-surface1 (phd-ark-get-color 'surface1)
                       (phd-ark-quantize-color (phd-ark-get-color 'surface1)))
         (phd-surface0 (phd-ark-get-color 'surface0)
                       (phd-ark-quantize-color (phd-ark-get-color 'surface0)))
         (phd-base     (phd-ark-get-color 'base)
                       (phd-ark-quantize-color (phd-ark-get-color 'base)))
         (phd-mantle   (phd-ark-get-color 'mantle)
                       (phd-ark-quantize-color (phd-ark-get-color 'mantle)))
         (phd-crust    (phd-ark-get-color 'crust)
                       (phd-ark-quantize-color (phd-ark-get-color 'crust)))

         (phd-current  (if (eq phd-ark-flavor 'light)
                           (phd-ark-darken (phd-ark-get-color 'base) 5)
                         (phd-ark-lighten (phd-ark-get-color 'base) 5))
                       (phd-ark-quantize-color
                        (if (eq phd-ark-flavor 'light)
                            (phd-ark-darken (phd-ark-get-color 'base) 5)
                          (phd-ark-lighten (phd-ark-get-color 'base) 5))))))

      (faces '(
               ;; Global
               (default             :background ,phd-base :foreground ,phd-text
                                    :font ,phd-ark-gui-font)
               (cursor              :background ,phd-blue)
               (fixed-pitch         :family ,phd-ark-terminal-font)
               (variable-pitch      :family ,phd-ark-gui-font)
               (escape-glyph        :foreground ,phd-orange)
               (minibuffer-prompt   :foreground ,phd-teal)
               (highlight           :background ,phd-overlay1) ;; :foreground ,phd-text)
               (hl-line             :inherit highlight)
               (region              :background ,phd-pink :foreground ,phd-text)
               (fringe              :inherit default :foreground ,phd-overlay0)
               (shadow              :foreground ,phd-overlay2)
               (secondary-selection :background ,phd-blue :foreground ,phd-text)
               (lazy-highlight      :inherit secondary-selection)
               (match               :inherit secondary-selection)
               (next-error          :inherit highlight)
               (query-replace       :inherit isearch)
               (trailing-whitespace :background ,phd-base)
               (linum               :background ,phd-base :foreground ,phd-overlay1)
               (tooltip             :inherit variable-pitch
                                    :background ,phd-overlay1 :foreground ,phd-text)
               (link                :underline (:color foreground-color :style line)
                                    :foreground ,phd-purple)
               (link-vistied        :underline (:color foreground-color :style line)
                                    :foreground ,phd-violet)
               (button              :inherit link)
               (error               :foreground ,phd-ruby)
               (warning             :foreground ,phd-orange)
               (success             :foreground ,phd-emerald)
               (show-paren-match    :background ,phd-blue :foreground ,phd-crust)
               (show-paren-mismatch :background ,phd-sand :foreground ,phd-crust)

               ;; Customize group
               (custom-state        :foreground ,phd-green)
               (widget-field        :background ,phd-overlay1)

               ;; Font-locks
               (font-lock-builtin-face           :foreground ,phd-cyan)
               (font-lock-preprocessor-face      :inherit font-lock-builtin-face)
               (font-lock-constant-face          :foreground ,phd-purple)
               (font-lock-function-name-face     :foreground ,phd-blue)
               (font-lock-keyword-face           :foreground ,phd-pink)
               (font-lock-type-face              :foreground ,phd-yellow)
               (font-lock-variable-name-face     :foreground ,phd-sand)
               (font-lock-string-face            :foreground ,phd-green)
               (font-lock-doc-face               :inherit font-lock-string-face)
               (font-lock-doc-string-face        :inherit font-lock-string-face)
               (font-lock-comment-face           :foreground ,phd-overlay2)
               (font-lock-comment-delimiter-face :inherit font-lock-comment-face)
               (font-lock-negation-char-face     nil)
               (font-lock-preprocessor-char-face nil)
               (font-lock-warning-face              :inherit warning)
               (font-lock-regexp-grouping-backslash :inherit (bold))
               (font-lock-regexp-grouping-construct :inherit (bold))

               ;; Org mode
               (org-block               :background ,phd-mantle)
               (org-block-background    :background ,phd-mantle)
               (org-code                :inherit org-block)

               ;; Org ref
               (org-ref-ref-face           :foreground ,phd-purple)
               (org-ref-cite-face          :foreground ,phd-cyan)
               (org-ref-label-face         :foreground ,phd-viridis)
               (org-ref-bad-cite-key-face  :foreground ,phd-pink)

               ;; Latex
               (font-latex-sedate-face       :foreground ,phd-pink)
               (font-latex-sectioning-1-face :inherit outline-1)
               (font-latex-sectioning-2-face :inherit outline-1)
               (font-latex-sectioning-3-face :inherit outline-2)
               (font-latex-sectioning-4-face :inherit outline-4)
               (font-latex-sectioning-5-face :inherit outline-5)
               (font-latex-italic-face       :foreground ,phd-green)
               (font-latex-math-face         :foreground ,phd-sand)
               (font-latex-warning-face      :foreground ,phd-blue)

               ;; JS2
               (js2-external-variable         :foreground ,phd-yellow)
               (js2-function-param            :foreground ,phd-violet)
               (js2-instance-member           :foreground ,phd-teal)
               (js2-jsdoc-html-tag-delimiter  :foreground ,phd-green)
               (js2-jsdoc-html-tag-name       :foreground ,phd-yellow)
               (js2-jsdoc-tag                 :foreground ,phd-purple)
               (js2-jsdoc-type                :foreground ,phd-teal)
               (js2-jsdoc-value               :foreground ,phd-violet)
               (js2-error                     :underline (:style wave :color ,phd-ruby))
               (js2-warning                   :underline (:style wave :color ,phd-orange))

               ;; Tab-line
               (tab-line               :background ,phd-surface0 :foreground ,phd-text
                                       :height 120)
               (tab-line-tab           :inherit tab-line :background ,phd-base)
               (tab-line-tab-current   :inherit tab-line-tab :foreground ,phd-teal)
               (tab-line-tab-inactive  :inherit tab-line :foreground ,phd-overlay0)
               (tab-line-highlight     :inherit tab-line
                                       :foreground ,phd-text :background ,phd-base)
               (tab-line-tab-modified  :inherit tab-line-current :foreground ,phd-blue)

               ;; Mode-line
               (mode-line              :background ,phd-surface0 :foreground ,phd-subtext1)
               (mode-line-highlight    :background ,phd-surface0 :foreground ,phd-subtext0)
               (mode-line-inactive     :background ,phd-mantle :foreground ,phd-text)
               (mode-line-emphasis     nil)
               (mode-line-buffer-id    nil)
               (header-line            :inherit mode-line)

               (phd-modeline-buffer-name-face       :foreground ,phd-teal)
               (phd-modeline-buffer-modified-face   :foreground ,phd-blue)
               (phd-modeline-buffer-read-only-face  :foreground ,phd-pink)
               (phd-modeline-buffer-line-face       :foreground ,phd-subtext1)
               (phd-modeline-buffer-column-face     :foreground ,phd-subtext1)
               (phd-modeline-buffer-percentage-face :foreground ,phd-subtext1)
               (phd-modeline-mode-face              :foreground ,phd-blue)
               (phd-modeline-flycheck-success-face  :foreground ,phd-grass)
               (phd-modeline-flycheck-warning-face  :foreground ,phd-orange)
               (phd-modeline-flycheck-error-face    :foreground ,phd-ruby)
               (phd-modeline-vc-icon-face           :foreground ,phd-pink)
               (phd-modeline-vc-branch-face         :foreground ,phd-sand)
               (phd-modeline-vc-status-face         :foreground ,phd-purple)
               (phd-modeline-mail-icon-face         :foreground ,phd-grass)
               (phd-modeline-mail-status-face       :foreground ,phd-text)
               (phd-modeline-inactive-face          :inherit mode-line-inactive)
               (phd-modeline-bar-face               :foreground ,phd-teal
                                                    :background ,phd-surface0)

               ;; Treemacs
               (treemacs-directory-face           :foreground ,phd-blue)
               (treemacs-file-face                :inherit default)
               (treemacs-root-face                :foreground ,phd-teal
                                                  :bold t
                                                  :height 1.1)
               (treemacs-git-added-face           :foreground ,phd-grass)
               (treemacs-git-modified-face        :foreground ,phd-pink)
               (treemacs-git-renamed-face         :foreground ,phd-teal)
               (treemacs-git-untracked-face       :foreground ,phd-cyan)
               (treemacs-git-conflict-face        :inherit error)
               (treemacs-all-the-icons-file-face  :foreground ,phd-blue)
               (treemacs-all-the-icons-root-face  :foreground ,phd-teal)

               ;; Perspeen
               (perspeen-tab--header-line-active  :background ,phd-blue :inherit mode-line)
               (perspeen-selected-face            :background ,phd-blue :inherit mode-line)

               ;; (i)Search
               (isearch                  :inherit region)
               (isearch-fail             :inherit highlight)
               (yas-field-highlight-face :inherit match)

               ;; Swiper
               (swiper-line-face     :background ,phd-blue :foreground ,phd-crust)
               (swiper-match-face-1  :background ,phd-crust :foreground ,phd-overlay0)
               (swiper-match-face-2  :background ,phd-sand :foreground ,phd-crust
                                     :bold t)
               (swiper-match-face-3  :background ,phd-pink :foreground ,phd-crust
                                     :bold t)
               (swiper-match-face-4  :background ,phd-green :foreground ,phd-crust
                                     :bold t)

               ;; Avy
               (avy-lead-face    :background ,phd-sand :foreground ,phd-base)
               (avy-lead-face-0  :background ,phd-red :foreground ,phd-base)
               (avy-lead-face-1  :background ,phd-viridis :foreground ,phd-base)
               (avy-lead-face-2  :background ,phd-blue :foreground ,phd-text)

               ;; Ace-window
               (aw-leading-char-face             :foreground ,phd-teal :bold t :height 2.0)
               (aw-minibuffer-leading-char-face  :foreground ,phd-teal :bold t :height 2.0)
               (aw-mode-line-face                :foreground ,phd-viridis :bold t)

               ;; Company
               (company-tooltip                  :inherit tooltip)
               (company-tooltip-common           :foreground ,phd-teal)
               (company-tooltip-common-selection :inherit company-tooltip-common)
               (company-tooltip-selection        :background ,phd-pink)
               (company-tooltip-search           :inherit secondary-selection)
               (company-tooltip-annotation       :foreground ,phd-cyan)
               (company-tooltip-mouse            :inherit secondary-selection)
               (company-scrollbar-bg             :background ,phd-teal)
               (company-scrollbar-fg             :background ,phd-ocean)
               (company-preview                  :inherit company-tooltip-selection)
               (company-preview-common           :inherit company-tooltip-common)
               (company-preview-search           :inherit company-tooltip-search)

                ;; which-key
               (which-key-key-face                   :foreground ,phd-green)
               (which-key-group-description-face     :foreground ,phd-violet)
               (which-key-command-description-face   :foreground ,phd-blue)
               (which-key-local-map-description-face :foreground ,phd-pink)

               ;; LSP
               (lsp-headerline-breadcrumb-symbols-face :foreground ,phd-teal)
               (lsp-headerline-breadcrumb-path-face :foreground ,phd-teal)
               (lsp-headerline-breadcrumb-path-error-face :foreground ,phd-surface1 :style nil)
               (lsp-headerline-breadcrumb-symbols-error-face :foreground ,phd-surface1 :style nil)
               (lsp-headerline-breadcrumb-path-warning-face :inherit lsp-headerline-breadcrumb-path-face)
               (lsp-headerline-breadcrumb-symbols-warning-face :inherit lsp-headerline-breadcrumb-symbols-face)
               (lsp-headerline-breadcrumb-deprecated-face :foreground ,phd-surface0 :style nil)
               (lsp-headerline-breadcrumb-path-info-face :foreground ,phd-blue)
               (lsp-headerline-breadcrumb-symbols-info-face :foreground ,phd-blue)
               (lsp-headerline-breadcrumb-project-prefix-face :foreground ,phd-blue :weight bold)
               (lsp-headerline-breadcrumb-unknown-project-prefix-face :foreground ,phd-cyan)
               (lsp-modeline-code-actions-preferred-face :foreground ,phd-yellow)
               (lsp-modeline-code-actions-face :foreground ,phd-cyan)
               (lsp-installation-buffer-face :foreground ,phd-green)
               (lsp-installation-finished-buffer-face :foreground ,phd-orange)

               ;; Flycheck/-make/-spell
               (flycheck-error       nil)
               (flycheck-warning     nil)
               (flycheck-info        nil)
               (flymake-errline      nil)
               (flyspell-incorrect   nil)
               (flyspell-duplicate   nil)
               ;; (flycheck-error     :underline (:style wave :color ,phd-red))
               ;; (flycheck-warning   :underline (:style wave :color ,phd-yellow))
               ;; (flycheck-info      :underline (:style wave :color ,phd-green))
               ;; (flyspell-incorrect :underline (:style wave :color ,phd-red)
               ;;                                      :inherit unspecified)

               ;; Highlight-indentation
               (highlight-indentation-face                :background ,phd-surface1)
               (highlight-indentation-current-column-mode :background ,phd-surface1)

               ;; Rainbow delimiters
               (rainbow-delimiters-depth-1-face   :foreground ,phd-blue)
               (rainbow-delimiters-depth-2-face   :foreground ,phd-pink)
               (rainbow-delimiters-depth-3-face   :foreground ,phd-green)
               (rainbow-delimiters-depth-4-face   :foreground ,phd-sand)
               (rainbow-delimiters-depth-5-face   :foreground ,phd-purple)
               (rainbow-delimiters-depth-6-face   :foreground ,phd-yellow)
               (rainbow-delimiters-depth-7-face   :foreground ,phd-cyan)
               (rainbow-delimiters-unmatched-face :foreground ,phd-overlay0
                                                  :bold t
                                                  :inverse-video t)

               ;; Bash
               (sh-heredoc              :foreground ,phd-violet)
               (sh-quoted-exec          :foreground ,phd-violet)

               ;; vterm
               (vterm                 :foreground ,phd-text :background ,phd-base)
               (vterm-color-default   :inherit default)
               (vterm-color-black     :foreground ,phd-crust :background ,phd-overlay1)
               (vterm-color-red       :foreground ,phd-red :background ,phd-ruby)
               (vterm-color-green     :foreground ,phd-green :background ,phd-viridis)
               (vterm-color-yellow    :foreground ,phd-yellow :background ,phd-sand)
               (vterm-color-blue      :foreground ,phd-blue :background ,phd-indigo)
               (vterm-color-magenta   :foreground ,phd-pink :background ,phd-magenta)
               (vterm-color-cyan      :foreground ,phd-teal :background ,phd-cyan)
               (vterm-color-white     :foreground ,phd-subtext0 :background ,phd-white)

               ;; message
               (message-header-name        :foreground ,phd-blue :bold t)
               (message-header-xheader     :foreground ,phd-teal)
               (message-header-newsgroups  :foreground ,phd-yellow :bold t)
               (message-header-to          :foreground ,phd-green :bold t)
               (message-header-cc          :foreground ,phd-green)
               (message-header-subject     :foreground ,phd-sand)
               (message-header-other       :foreground ,phd-pink)
               (message-mml                :foreground ,phd-cyan)
               (message-separator          :foreground ,phd-overlay0)
               (message-cited-text-1       :foreground ,phd-violet)
               (message-cited-text-2       :foreground ,phd-purple)
               (message-cited-text-3       :foreground ,phd-ocean)
               (message-cited-text-4       :foreground ,phd-overlay0)

               ;; gnus
               (gnus-header-name           :foreground ,phd-blue :bold t)
               (gnus-header-from           :foreground ,phd-green :bold t)
               (gnus-header-subject        :foreground ,phd-sand)
               (gnus-header-content        :foreground ,phd-pink)
               (gnus-header-newsgroups     :foreground ,phd-yellow)
               (gnus-cite-1                :foreground ,phd-violet)
               (gnus-cite-2                :foreground ,phd-purple)
               (gnus-cite-3                :foreground ,phd-ocean)
               (gnus-cite-4                :foreground ,phd-overlay0)
               (gnus-cite-5                :foreground ,phd-violet)
               (gnus-cite-6                :foreground ,phd-purple)
               (gnus-cite-7                :foreground ,phd-indigo)
               (gnus-cite-8                :foreground ,phd-overlay0)
               (gnus-cite-9                :foreground ,phd-violet)
               (gnus-cite-10               :foreground ,phd-purple)
               (gnus-cite-11               :foreground ,phd-blue)
               (gnus-signature             :foreground ,phd-cyan)

               ;; mu4e
               (mu4e-header-face                  :inherit default)
               (mu4e-title-face                   :foreground ,phd-sand :bold t)
               (mu4e-highlight-face               :foreground ,phd-viridis :bold t)
               (mu4e-header-title-face            :foreground ,phd-pink)
               (mu4e-header-key-face              :foreground ,phd-blue :bold t)
               (mu4e-header-highlight-face        :inherit highlight)
               (mu4e-header-value-face            :foreground ,phd-pink)
               (mu4e-special-header-value-face    :foreground ,phd-sand)
               (mu4e-contact-face                 :foreground ,phd-green)
               (mu4e-compose-header-face          :foreground ,phd-green :bold t)
               (mu4e-attach-number-face           :foreground ,phd-cyan :bold t)
               (mu4e-footer-face                  :foreground ,phd-cyan)
               (mu4e-compose-separator-face       :foreground ,phd-overlay0)
               (mu4e-context-face                 :foreground ,phd-blue :bold t)
               (mu4e-unread-face                  :foreground ,phd-blue :bold t)
               (mu4e-forwarded-face               :foreground ,phd-purple)
               (mu4e-draft-face                   :foreground ,phd-pink)
               (mu4e-replied-face                 :foreground ,phd-viridis)
               (mu4e-flagged-face                 :foreground ,phd-sand)
               (mu4e-header-marks-face            :foreground ,phd-cyan)
               (mu4e-region-code                  :foreground ,phd-violet)
               (mu4e-link-face                    :inherit link)
               (mu4e-cited-1-face                 :foreground ,phd-violet)
               (mu4e-cited-2-face                 :foreground ,phd-purple)
               (mu4e-cited-3-face                 :foreground ,phd-ocean)
               (mu4e-cited-4-face                 :foreground ,phd-overlay0)
               (mu4e-cited-5-face                 :foreground ,phd-violet)
               (mu4e-cited-6-face                 :foreground ,phd-purple)
               (mu4e-cited-7-face                 :foreground ,phd-indigo))))

  (apply #'custom-theme-set-faces
         'phd-ark
         (let* ((expand-with-func
                 (lambda (func spec)
                   (let (reduced-color-list)
                     (dolist (col colors reduced-color-list)
                       (push (list (car col) (funcall func col))
                             reduced-color-list))
                     (eval `(let ,reduced-color-list
                              (backquote ,spec))))))
                whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (t                       ; terminal with 256 colors
                       ,(funcall expand-with-func '(lambda (v) (cadr (cdr v))) spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'phd-ark
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  (_ 'cadr))))                ; terminal withs 256 colors
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get (if (eq phd-ark-flavor 'light)
                                                 'phd-text
                                               'phd-crust) colors))
               ,(funcall get-func (alist-get 'phd-red colors))
               ,(funcall get-func (alist-get 'phd-green colors))
               ,(funcall get-func (alist-get 'phd-yellow colors))
               ,(funcall get-func (alist-get 'phd-blue colors))
               ,(funcall get-func (alist-get 'phd-pink colors))
               ,(funcall get-func (alist-get 'phd-cyan colors))
               ,(funcall get-func (alist-get (if (eq phd-ark-flavor 'light)
                                                 'phd-crust
                                               'phd-text) colors))]))
           `((rustic-ansi-faces
              (vector
               ,(funcall get-func (alist-get (if (eq phd-ark-flavor 'light)
                                                 'phd-text
                                               'phd-crust) colors))
               ,(funcall get-func (alist-get 'phd-red colors))
               ,(funcall get-func (alist-get 'phd-green colors))
               ,(funcall get-func (alist-get 'phd-yellow colors))
               ,(funcall get-func (alist-get 'phd-blue colors))
               ,(funcall get-func (alist-get 'phd-pink colors))
               ,(funcall get-func (alist-get 'phd-cyan colors))
               ,(funcall get-func (alist-get (if (eq phd-ark-flavor 'light)
                                                 'phd-crust
                                               'phd-text) colors))))))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'phd-ark)

;; Unbind functions used for internal use
(fmakunbound 'phd-ark-quantize-color)
(fmakunbound 'phd-ark-lighten-color)
(fmakunbound 'phd-ark-darken-color)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; phd-ark-theme.el ends here
