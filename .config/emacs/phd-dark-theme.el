;;; phd-dark.el ---  phdenzel's dark Emacs theme
;;; Commentary:
;;; Should also work in Terminal.

;;; Code:

(deftheme phd-dark
  "phdenzel's dark Emacs theme; partially compatible with use in Terminal."
  )

(defvar phd-dark-color-palette
  (let* ((256color  (eq (display-color-cells (selected-frame)) 256))
         (colors `(("fg"        .  (if ,256color "#C6C6C6" "#BDC3CE"))
                   ("fg-b"      .  (if ,256color "#D0D0D0" "#DDE3EE"))
                   ("fg-bb"     .  (if ,256color "#DADADA" "#EDF3FE"))
                   ("bg"        .  "#303033")
                   ("bg-b"      .  (if ,256color "#3A3A3A" "#404850"))
                   ("bg-bb"     .  (if ,256color "#444444" "#3A3C3F"))
                   ("white"     .  (if ,256color "#CCCCCC" "#DFDFDF"))
                   ("grey"      .  (if ,256color "#666666" "#5B6268"))
                   ("grey-b"    .  (if ,256color "#999999" "#8A8A8A"))
                   ("grey-d"    .  (if ,256color "#444444" "#464858"))
                   
                   ("black"     .  (if ,256color "#000000" "#181E26"))
                   ("red"       .  (if ,256color "#FF5F5F" "#FF6D6B"))
                   ("red-d"     .  "#D7005F")
                   ("red-dd"    .  "#D70000")
                   ("green"     .  (if ,256color "#00AF87" "#44BC84"))
                   ("green-d"   .  (if ,256color "#5FAF87" "#3DAA77"))
                   ("green-b"   .  "#00AF5F")
                   ("yellow"    .  "#FFD787")
                   ("orange"    .  (if ,256color "#FFAF5F" "#FDB760"))
                   ("orange-d"  .  "#FF5F00")
                   ("blue"      .  (if ,256color "#5F87FF" "#5F8AF7"))
                   ("blue-d"    .  (if ,256color "#005f87" "#1F5582"))
                   ("blue-dd"   .  (if ,256color "#0000FF" "#3723B7"))
                   ("violet"    .  (if ,256color "#AFAFD7" "#A9A1E1"))
                   ("purple"    .  "#8787FF")
                   ("magenta"   .  (if ,256color "#FF0087" "#E83A82"))
                   ("cyan"      .  (if ,256color "#5FD7FF" "#46D9FF"))
                   ("teal"      .  (if ,256color "#5FAFAF" "#4DB5BD"))
                   
                   ("gui-font"      . "Inconsolata")
                   ("terminal-font" . "Fira Mono")
                   )))
    colors)
  "Color palette alist for the phd-dark theme.")

(defmacro with-phd-variables (&rest body)
  "Provides color variables from phd-dark-color-palette in the BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   phd-dark-color-palette))
     ,@body))

(with-phd-variables
  (custom-theme-set-faces
  'phd-dark

  ;; Global
  `(default             ((t (:background ,bg :foreground ,fg))))
  `(cursor              ((t (:background ,blue))))
  `(fixed-pitch         ((t (:family ,terminal-font))))
  `(variable-pitch      ((t (:family ,gui-font))))
  `(escape-glyph        ((t (:foreground ,orange-d))))
  `(minibuffer-prompt   ((t (:foreground ,teal))))
  `(highlight           ((t (:background ,grey-d)))) ;; :foreground ,fg))))
  `(hl-line             ((t (:inherit highlight))))
  `(region              ((t (:background ,magenta :foreground ,fg))))
  `(fringe              ((t (:inherit (default) :foreground ,grey))))
  `(shadow              ((t (:foreground ,grey-b))))
  `(secondary-selection ((t (:background ,blue :foreground ,fg))))
  `(lazy-highlight      ((t (:inherit secondary-selection))))
  `(match               ((t (:inherit secondary-selection))))
  `(next-error          ((t (:inherit highlight))))
  `(query-replace       ((t (:inherit isearch))))
  `(trailing-whitespace ((t (:background ,bg))))
  '(linum               ((t (:background ,bg :foreground ,grey-d))))
  `(tooltip             ((t (:inherit (variable-pitch)
                                      :background ,grey-d :foreground ,fg))))
  `(link                ((t (:underline (:color foreground-color :style line)
                                        :foreground ,purple))))
  `(link-vistied        ((t (:underline (:color foreground-color :style line)
                                        :foreground ,violet))))
  `(button              ((t (:inherit link))))
  `(error               ((t (:foreground ,red-d))))
  `(warning             ((t (:foreground ,orange-d))))
  `(success             ((t (:foreground ,green-b))))

  ;; Font-locks
  `(font-lock-builtin-face           ((t (:foreground ,cyan))))
  `(font-lock-preprocessor-face      ((t (:inherit (font-lock-builtin-face)))))
  `(font-lock-constant-face          ((t (:foreground ,purple))))
  `(font-lock-function-name-face     ((t (:foreground ,blue))))
  `(font-lock-keyword-face           ((t (:foreground ,magenta))))
  `(font-lock-type-face              ((t (:foreground ,yellow))))
  `(font-lock-variable-name-face     ((t (:foreground ,orange))))
  `(font-lock-string-face            ((t (:foreground ,green))))
  `(font-lock-doc-face               ((t (:inherit font-lock-string-face))))
  `(font-lock-doc-string-face        ((t (:inherit font-lock-string-face))))
  `(font-lock-comment-face           ((t (:foreground ,grey-b))))
  `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
  `(font-lock-negation-char-face     ((t nil)))
  `(font-lock-preprocessor-char-face ((t nil)))
  `(font-lock-warning-face           ((t (:inherit (warning)))))
  `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
  `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

  ;; Latex
  `(font-latex-sedate-face       ((t (:foreground ,magenta))))
  `(font-latex-sectioning-1-face ((t (:foreground ,purple))))
  `(font-latex-sectioning-2-face ((t (:foreground ,yellow))))
  `(font-latex-sectioning-3-face ((t (:foreground ,green))))
  `(font-latex-italic-face ((t (:foreground ,green))))
  `(font-latex-math-face ((t (:foreground ,orange))))
  `(font-latex-warning-face ((t (:foreground ,blue))))

  ;; Mode-line
  `(mode-line                ((t (:background ,bg-bb :foreground ,fg-bb))))
  `(mode-line-inactive       ((t (:background ,bg-b :foreground ,fg-bb))))
  `(mode-line-emphasis       ((t (:bold t))))
  `(mode-line-buffer-id      ((t (:bold t))))
  `(persp-selected-face      ((t (:foreground ,purple))))
  `(header-line              ((t (:inherit mode-line))))
  `(powerline-active1        ((t (:inherit mode-line))))
  `(powerline-active2        ((t (:inherit mode-line))))
  `(powerline-inactive1      ((t (:inherit mode-line-inactive))))
  `(powerline-inactive2      ((t (:inherit mode-line-inactive))))
  `(phd-modeline-buffer-path       ((t (:foreground ,teal))))
  `(phd-modeline-buffer-project    ((t (:foreground ,fg))))
  `(phd-modeline-buffer-modified   ((t (:foreground ,blue))))
  `(phd-modeline-buffer-major-mode ((t (:foreground ,blue))))
  `(phd-modeline-highlight         ((t (:foreground ,blue))))
  `(phd-modeline-bar               ((t (:background ,blue))))
  `(phd-modeline-eldoc-bar         ((t (:background ,yellow))))

  ;; Perspeen
  `(perspeen-tab--header-line-active  ((t (:background ,blue :inherit mode-line))))
  `(perspeen-selected-face            ((t (:background ,blue :inherit mode-line))))

  ;; (i)Search
  `(isearch                  ((t (:inherit region))))
  `(isearch-fail             ((t (:inherit highlight))))
  `(yas-field-highlight-face ((t (:inherit match))))

  ;; Avy
  `(avy-lead-face      ((t (:background ,violet :foreground ,bg))))
  `(avy-lead-face-0    ((t (:background ,magenta :foreground ,fg))))
  `(avy-lead-face-1    ((t (:background ,blue :foreground ,fg))))
  `(avy-lead-face-2    ((t (:background ,orange :foreground ,fg))))

  ;; Company
  `(company-tooltip                  ((t (:inherit tooltip))))
  `(company-tooltip-common           ((t (:foreground ,teal))))
  `(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
  `(company-tooltip-selection        ((t (:background ,magenta))))
  `(company-tooltip-search           ((t (:inherit secondary-selection))))
  `(company-tooltip-annotation       ((t (:foreground ,cyan))))
  `(company-tooltip-mouse            ((t (:inherit secondary-selection))))
  `(company-scrollbar-bg             ((t (:background ,teal))))
  `(company-scrollbar-fg             ((t (:background ,blue-d))))
  `(company-preview                  ((t (:inherit company-tooltip-selection))))
  `(company-preview-common           ((t (:inherit company-tooltip-common))))
  `(company-preview-search           ((t (:inherit company-tooltip-search))))

  ;; which-key
  `(which-key-key-face                   ((t (:foreground ,green))))
  `(which-key-group-description-face     ((t (:foreground ,violet))))
  `(which-key-command-description-face   ((t (:foreground ,blue))))
  `(which-key-local-map-description-face ((t (:foreground ,magenta))))

  ;; Flycheck/-make/-spell
  `(flycheck-error     ((t nil)))
  `(flycheck-warning   ((t nil)))
  `(flycheck-info      ((t nil)))
  `(flymake-errline    ((t nil)))
  `(flyspell-incorrect ((t nil)))
  `(flyspell-duplicate ((t nil)))
  ;; `(flycheck-error     ((t (:underline (:style wave :color ,red)))))
  ;; `(flycheck-warning   ((t (:underline (:style wave :color ,yellow)))))
  ;; `(flycheck-info      ((t (:underline (:style wave :color ,green)))))
  ;; `(flyspell-incorrect ((t (:underline (:style wave :color ,red)
  ;;                                      :inherit unspecified))))

  ;; Swiper
  `(swiper-line-face      ((t (:background ,blue :foreground ,black))))
  `(swiper-match-face-1   ((t (:background ,black :foreground ,grey))))
  `(swiper-match-face-2   ((t (:background ,orange :foreground ,black
                                           :bold t))))
  `(swiper-match-face-3   ((t (:background ,magenta :foreground ,black
                                           :bold t))))
  `(swiper-match-face-4   ((t (:background ,green :foreground ,black
                                           :bold t))))

  ;; Highlight-indentation
  `(highlight-indentation-face                ((t (:background ,bg-b))))
  `(highlight-indentation-current-column-mode ((t (:background ,bg-b))))

  ;; Rainbow delimiters
  `(rainbow-delimiters-depth-1-face   ((t (:foreground ,blue))))
  `(rainbow-delimiters-depth-2-face   ((t (:foreground ,magenta))))
  `(rainbow-delimiters-depth-3-face   ((t (:foreground ,green))))
  `(rainbow-delimiters-depth-4-face   ((t (:foreground ,orange))))
  `(rainbow-delimiters-depth-5-face   ((t (:foreground ,purple))))
  `(rainbow-delimiters-depth-6-face   ((t (:foreground ,yellow))))
  `(rainbow-delimiters-depth-7-face   ((t (:foreground ,cyan))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,grey
                                                       :bold t
                                                       :inverse-video t))))

  ;; Bash
  `(sh-quoted-exec          ((t (:foreground ,violet))))

  ))

(with-phd-variables
 (custom-set-variables
  ;;'phd-dark

  ;; highlight-parentheses
  `(hl-paren-colors '(,blue ,magenta ,green ,orange ,purple ,grey ,yellow ))
  `(highlight-parentheses-colors '(,blue ,magenta ,green ,orange ,purple ,grey ,yellow ))
  )
)

(provide-theme 'phd-dark)
;;(((((((((((((())))))))))))))
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; phd-dark-theme.el ends here
