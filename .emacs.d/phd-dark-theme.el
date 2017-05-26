;;; phd-dark.el ---  phdenzel's dark Emacs theme
;;; Commentary:
;; Should also work in Terminal.

;;; Code:

(deftheme phd-dark
  "phdenzel's dark Emacs theme; partially compatible with use in Terminal."
  )

(defvar phd-dark-color-palette
  '(("bg"        .  (if window-system "#2A2F38" "unspecified-bg"))
    ("fg"        .  (if window-system "#BDC3CE" "unspecified-fg"))
    ("bg-b"      .  (if window-system "#404850" "unspecified-bg"))
    ("fg-b"      .  (if window-system "#BDC3CE" "unspecified-fg"))
    ("bg-bb"     .  (if window-system "#3A3C3F" "unspecified-bg"))
    ("fg-bb"     .  (if window-system "#BDC3CE" "unspecified-fg"))
    ("black"     .  (if window-system "#181E26" "#000000"))
    ("white"     .  (if window-system "#DFDFDF" "#D7D7D7"))
    ("grey-b"    .  (if window-system "#8A8A8A" "#878787"))
    ("grey"      .  (if window-system "#5B6268" "#525252"))
    ("grey-d"    .  (if window-system "#464858" "#464858"))
    ("yellow"    .  (if window-system "#FFD787" "#FFD787"))
    ("orange"    .  (if window-system "#FDB760" "#FFAF5F"))
    ("orange-d"  .  (if window-system "#FF5F00" "#FF5F00"))
    ("red"       .  (if window-system "#FF6D6B" "#FF5F5F"))
    ("red-d"     .  (if window-system "#D7005F" "#D7005F"))
    ("red-dd"    .  (if window-system "#D70000" "#D70000"))
    ("magenta"   .  (if window-system "#E83A82" "#FF0087"))
    ("purple"    .  (if window-system "#8787FF" "#8787FF"))
    ("violet"    .  (if window-system "#A9A1E1" "#AFAFD7"))
    ("cyan"      .  (if window-system "#46D9FF" "#5FD7FF"))
    ("teal"      .  (if window-system "#4DB5BD" "#5FAFAF"))
    ("blue"      .  (if window-system "#5F8AF7" "#5F87FF"))
    ("blue-d"    .  (if window-system "#1F5582" "#005f87"))
    ("blue-dd"   .  (if window-system "#3723B7" "#0000FF"))
    ("green"     .  (if window-system "#3DAA77" "#00875F"))
    ("green-b"   .  (if window-system "#00AF5F" "#00AF5F"))
    ("gui-font"      . "Hack")
    ("terminal-font" . "Fira Mono")
    )
  "Color palette alist for the phd-dark theme."
  )

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
  `(minibuffer-prompt   ((t (:foreground ,blue-dd))))
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
  `(font-lock-comment-face           ((t (:foreground ,grey))))
  `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
  `(font-lock-negation-char-face     ((t nil)))
  `(font-lock-preprocessor-char-face ((t nil)))
  `(font-lock-warning-face           ((t (:inherit (warning)))))
  `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
  `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

  ;; Latex
  `(font-latex-sedate-face       ((t (:foreground ,magenta))))
  `(font-latex-sectioning-2-face ((t (:foreground ,orange))))

  ;; Mode-line
  `(mode-line                ((t (:background ,bg-bb :foreground ,fg-bb))))
  `(mode-line-inactive       ((t (:background ,bg-b :foreground ,fg-b))))
  `(mode-line-emphasis       ((t (:bold t))))
  `(mode-line-buffer-id      ((t (:bold t))))
  `(persp-selected-face      ((t (:foreground ,purple))))
  `(header-line              ((t (:inherit mode-line))))
  `(powerline-active1        ((t (:inherit mode-line))))
  `(powerline-active2        ((t (:inherit mode-line))))
  `(powerline-inactive1      ((t (:inherit mode-line-inactive))))
  `(powerline-inactive2      ((t (:inherit mode-line-inactive))))
  `(phd-modeline-buffer-path ((t (:foreground ,teal))))
  `(phd-modeline-buffer-project    ((t (:foreground ,fg))))
  `(phd-modeline-buffer-modified   ((t (:foreground ,blue))))
  `(phd-modeline-buffer-major-mode ((t (:foreground ,blue))))
  `(phd-modeline-highlight         ((t (:foreground ,blue))))
  `(phd-modeline-bar               ((t (:background ,blue))))
  `(phd-modeline-eldoc-bar         ((t (:background ,yellow))))

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
  `(company-preview                 ((t (:inherit company-tooltip-selection))))
  `(company-preview-common          ((t (:inherit company-tooltip-common))))
  `(company-preview-search          ((t (:inherit company-tooltip-search))))

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

  ;; Hl-paren
  `(hl-paren-colors '(,blue ,magenta ,green ,orange
                            ,violet ,purple ,red ,red-d ,red-dd))

  ;; Highlight-indentation
  `(highlight-indentation-face                ((t (:background ,bg-b))))
  `(highlight-indentation-current-column-mode ((t (:background ,bg-b))))

  ;; Rainbow delimiters
  `(rainbow-delimiters-depth-1-face   ((t (:foreground ,blue))))
  `(rainbow-delimiters-depth-2-face   ((t (:foreground ,magenta))))
  `(rainbow-delimiters-depth-3-face   ((t (:foreground ,green))))
  `(rainbow-delimiters-depth-4-face   ((t (:foreground ,orange))))
  `(rainbow-delimiters-depth-5-face   ((t (:foreground ,violet))))
  `(rainbow-delimiters-unmatched-face ((t (:foreground ,red
                                                       :bold t
                                                       :inverse-video t))))

  ;; Bash
  `(sh-quoted-exec          ((t (:foreground ,violet))))

  ))

(if window-system
    (setq hl-paren-colors '(
                            "#E83A82" ;; magenta
                            "#FDB760" ;; orange
                            "#3DAA77" ;; green
                            "#5F8AF7" ;; blue
                            "#D70000" ;; red-dd
                            "#8787FF" ;; purple
                            "#D7005F" ;; red-d
                            "#A9A1E1" ;; violet
                            "#FF6D6B" ;; red
                            )
          )
  (setq hl-paren-colors '(
                          "#FF0087" ;; magenta
                          "#FFAF5F" ;; orange
                          "#00875F" ;; green
                          "#5F87FF" ;; blue
                          "#D70000" ;; red-dd
                          "#8787FF" ;; purple
                          "#D7005F" ;; red-d
                          "#AFAFD7" ;; violet
                          "#FF5F5F" ;; red
                          )
  )

(global-highlight-parentheses-mode)
)

(provide-theme 'phd-dark)
;;; phd-dark-theme.el ends here
