;;; phd-dark.el ---  phdenzel's dark Emacs theme
;;; Commentary:
;;; Should also work in Terminal.

;;; Code:

(deftheme phd-dark
  "phdenzel's dark Emacs theme; partially compatible with use in Terminal."
  )

(defvar phd-dark-color-palette
  (let* ((colors `(("fg"        .  "#BDC3CE")
                   ("fg-b"      .  "#DDE3EE")
                   ("fg-bb"     .  "#EDF3FE")
                   ("bg"        .  "#303033")
                   ("bg-d"      .  "#2B2B2F")
                   ("bg-b"      .  "#404850")
                   ("bg-bb"     .  "#3A3C3F")
                   ("white"     .  "#DFDFDF")
                   ("white-b"   .  "#EDF3FE")
                   ("grey"      .  "#5B6268")
                   ("grey-b"    .  "#8A8A8A")
                   ("grey-d"    .  "#464858")
                   ("black"     .  "#181E26")
                   
                   ("red"       .  "#FF6D6B")
                   ("red-d"     .  "#D7005F")
                   ("red-dd"    .  "#D70000")
                   ("green"     .  "#44BC84")
                   ("green-d"   .  "#3DAA77")
                   ("green-b"   .  "#00AF5F")
                   ("green-bb"  .  "#00AF87")
                   ("yellow"    .  "#FFD787")
                   ("orange"    .  "#FDB760")
                   ("orange-b"  .  "#F69927")
                   ("orange-d"  .  "#FF5F00")
                   ("blue"      .  "#5F8AF7")
                   ("blue-b"    .  "#5F5FFF")
                   ("blue-d"    .  "#1F5582")
                   ("blue-dd"   .  "#3723B7")
                   ("violet"    .  "#A9A1E1")
                   ("purple"    .  "#8787FF")
                   ("magenta"   .  "#E83A82")
                   ("magenta-d" .  "#FF0087")
                   ("cyan"      .  "#46D9FF")
                   ("teal"      .  "#4DB5BD")
                                      
                   ("gui-font"      . "Fira Mono-13")
                   ("terminal-font" . "Fira Mono-11")
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
  `(default             ((t (:background ,bg :foreground ,fg :font ,gui-font))))
  `(cursor              ((t (:background ,blue))))
  `(fixed-pitch         ((t (:family ,terminal-font))))
  `(variable-pitch      ((t (:family ,gui-font))))
  `(escape-glyph        ((t (:foreground ,orange-b))))
  `(minibuffer-prompt   ((t (:foreground ,teal))))
  `(highlight           ((t (:background ,grey-d)))) ;; :foreground ,fg))))
  `(hl-line             ((t (:inherit highlight))))
  `(region              ((t (:background ,magenta :foreground ,fg))))
  `(fringe              ((t (:inherit default :foreground ,grey))))
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
  `(warning             ((t (:foreground ,orange-b))))
  `(success             ((t (:foreground ,green-b))))
  `(show-paren-match    ((t (:background ,blue :foreground ,black))))
  `(show-paren-mismatch ((t (:background ,orange :foreground ,black))))

  ;; Customize group
  `(custom-state        ((t (:foreground ,green))))
  `(widget-field        ((t (:background ,grey-d))))

  ;; Font-locks
  `(font-lock-builtin-face           ((t (:foreground ,cyan))))
  `(font-lock-preprocessor-face      ((t (:inherit font-lock-builtin-face))))
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
  `(font-lock-warning-face           ((t (:inherit warning))))
  `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
  `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))

  ;; Org mode
  `(org-block               ((t (:background ,bg-d))))
  `(org-block-background    ((t (:background ,bg-d))))
  `(org-code                ((t (:inherit org-block))))

  ;; Org ref
  `(org-ref-ref-face           ((t (:foreground ,purple))))
  `(org-ref-cite-face          ((t (:foreground ,cyan))))
  `(org-ref-label-face         ((t (:foreground ,green-bb))))
  `(org-ref-bad-cite-key-face  ((t (:foreground ,magenta))))

  ;; Latex
  `(font-latex-sedate-face       ((t (:foreground ,magenta))))
  `(font-latex-sectioning-1-face ((t (:inherit outline-1))))
  `(font-latex-sectioning-2-face ((t (:inherit outline-1))))
  `(font-latex-sectioning-3-face ((t (:inherit outline-2))))
  `(font-latex-sectioning-4-face ((t (:inherit outline-4))))
  `(font-latex-sectioning-5-face ((t (:inherit outline-5))))
  `(font-latex-italic-face ((t (:foreground ,green))))
  `(font-latex-math-face ((t (:foreground ,orange))))
  `(font-latex-warning-face ((t (:foreground ,blue))))

  ;; JS2
  `(js2-external-variable         ((t (:foreground ,yellow))))
  `(js2-function-param            ((t (:foreground ,violet))))
  `(js2-instance-member           ((t (:foreground ,teal))))
  `(js2-jsdoc-html-tag-delimiter  ((t (:foreground ,green))))
  `(js2-jsdoc-html-tag-name       ((t (:foreground ,yellow))))
  `(js2-jsdoc-tag                 ((t (:foreground ,purple))))
  `(js2-jsdoc-type                ((t (:foreground ,teal))))
  `(js2-jsdoc-value               ((t (:foreground ,violet))))
  `(js2-error                     ((t (:underline (:style wave :color ,red-d)))))
  `(js2-warning                   ((t (:underline (:style wave :color ,orange-b)))))

  ;; Mode-line
  `(mode-line                ((t (:background ,bg-bb :foreground ,fg-b))))
  `(mode-line-highlight      ((t (:background ,bg-bb :foreground ,fg-bb))))
  `(mode-line-inactive       ((t (:background ,bg-d :foreground ,fg))))
  `(mode-line-emphasis       ((t nil)))
  `(mode-line-buffer-id      ((t nil)))
  `(header-line              ((t (:inherit mode-line))))

  `(phd-modeline-buffer-name-face       ((t (:foreground ,teal))))
  `(phd-modeline-buffer-modified-face   ((t (:foreground ,blue))))
  `(phd-modeline-buffer-read-only-face  ((t (:foreground ,magenta))))
  `(phd-modeline-buffer-line-face       ((t (:foreground ,fg-b))))
  `(phd-modeline-buffer-column-face     ((t (:foreground ,fg-b))))
  `(phd-modeline-buffer-percentage-face ((t (:foreground ,fg-b))))
  `(phd-modeline-mode-face              ((t (:foreground ,blue))))
  `(phd-modeline-flycheck-success-face  ((t (:foreground ,green-d))))
  `(phd-modeline-flycheck-warning-face  ((t (:foreground ,orange-b))))
  `(phd-modeline-flycheck-error-face    ((t (:foreground ,red-d))))
  `(phd-modeline-vc-icon-face           ((t (:foreground ,magenta))))
  `(phd-modeline-vc-branch-face         ((t (:foreground ,orange))))
  `(phd-modeline-vc-status-face         ((t (:foreground ,purple))))
  `(phd-modeline-mail-icon-face         ((t (:foreground ,green-d))))
  `(phd-modeline-mail-status-face       ((t (:foreground ,fg))))
  `(phd-modeline-inactive-face          ((t (:inherit 'mode-line-inactive))))
  `(phd-modeline-bar-face               ((t (:foreground ,teal :background ,bg-bb))))
  

  ;; Perspeen
  `(perspeen-tab--header-line-active  ((t (:background ,blue :inherit mode-line))))
  `(perspeen-selected-face            ((t (:background ,blue :inherit mode-line))))

  ;; (i)Search
  `(isearch                  ((t (:inherit region))))
  `(isearch-fail             ((t (:inherit highlight))))
  `(yas-field-highlight-face ((t (:inherit match))))

  ;; Swiper
  `(swiper-line-face      ((t (:background ,blue :foreground ,black))))
  `(swiper-match-face-1   ((t (:background ,black :foreground ,grey))))
  `(swiper-match-face-2   ((t (:background ,orange :foreground ,black
                                           :bold t))))
  `(swiper-match-face-3   ((t (:background ,magenta :foreground ,black
                                           :bold t))))
  `(swiper-match-face-4   ((t (:background ,green :foreground ,black
                                           :bold t))))

  ;; 

  ;; Avy
  `(avy-lead-face      ((t (:background ,orange :foreground ,bg))))
  `(avy-lead-face-0    ((t (:background ,red :foreground ,bg))))
  `(avy-lead-face-1    ((t (:background ,green-bb :foreground ,bg))))
  `(avy-lead-face-2    ((t (:background ,blue :foreground ,fg))))

  ;; Ace-window
  `(aw-leading-char-face             ((t (:foreground ,teal :bold t :height 2.0))))
  `(aw-minibuffer-leading-char-face  ((t (:foreground ,teal :bold t :height 2.0))))
  `(aw-mode-line-face                ((t (:foreground ,green-bb :bold t))))

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
  `(sh-heredoc              ((t (:foreground ,violet))))
  `(sh-quoted-exec          ((t (:foreground ,violet))))

  ;; vterm
  `(vterm                 ((t (:foreground ,fg :background ,bg))))
  `(vterm-color-default   ((t (:inherit default))))
  `(vterm-color-black     ((t (:foreground ,black :background ,grey-d))))
  `(vterm-color-red       ((t (:foreground ,red :background ,red-d))))
  `(vterm-color-green     ((t (:foreground ,green :background ,green-bb))))
  `(vterm-color-yellow    ((t (:foreground ,yellow :background ,orange))))
  `(vterm-color-blue      ((t (:foreground ,blue :background ,blue-b))))
  `(vterm-color-magenta   ((t (:foreground ,magenta :background ,magenta-d))))
  `(vterm-color-cyan      ((t (:foreground ,teal :background ,cyan))))
  `(vterm-color-white     ((t (:foreground ,white :background ,white-b))))

  ;; message
  `(message-header-name        ((t (:foreground ,blue :bold t))))
  `(message-header-xheader     ((t (:foreground ,teal))))
  `(message-header-newsgroups  ((t (:foreground ,yellow :bold t))))
  `(message-header-to          ((t (:foreground ,green :bold t))))
  `(message-header-cc          ((t (:foreground ,green))))
  `(message-header-subject     ((t (:foreground ,orange))))
  `(message-header-other       ((t (:foreground ,magenta))))
  `(message-mml                ((t (:foreground ,cyan))))
  `(message-separator          ((t (:foreground ,grey))))
  `(message-cited-text-1       ((t (:foreground ,violet))))
  `(message-cited-text-2       ((t (:foreground ,purple))))
  `(message-cited-text-3       ((t (:foreground ,blue-d))))
  `(message-cited-text-4       ((t (:foreground ,grey))))

  ;; gnus
  `(gnus-header-name           ((t (:foreground ,blue :bold t))))
  `(gnus-header-from           ((t (:foreground ,green :bold t))))
  `(gnus-header-subject        ((t (:foreground ,orange))))
  `(gnus-header-content        ((t (:foreground ,magenta))))
  `(gnus-header-newsgroups     ((t (:foreground ,yellow))))
  `(gnus-cite-1                ((t (:foreground ,violet))))
  `(gnus-cite-2                ((t (:foreground ,purple))))
  `(gnus-cite-3                ((t (:foreground ,blue-d))))
  `(gnus-cite-4                ((t (:foreground ,grey))))
  `(gnus-cite-5                ((t (:foreground ,violet))))
  `(gnus-cite-6                ((t (:foreground ,purple))))
  `(gnus-cite-7                ((t (:foreground ,blue-b))))
  `(gnus-cite-8                ((t (:foreground ,grey))))
  `(gnus-cite-9                ((t (:foreground ,violet))))
  `(gnus-cite-10               ((t (:foreground ,purple))))
  `(gnus-cite-11               ((t (:foreground ,blue))))
  `(gnus-signature             ((t (:foreground ,cyan))))

  ;; mu4e
  `(mu4e-header-face                  ((t (:inherit default))))
  `(mu4e-title-face                   ((t (:foreground ,orange :bold t))))
  `(mu4e-highlight-face               ((t (:foreground ,green-bb :bold t))))
  `(mu4e-header-title-face            ((t (:foreground ,magenta))))
  `(mu4e-header-key-face              ((t (:foreground ,blue :bold t))))
  `(mu4e-header-highlight-face        ((t (:inherit highlight))))
  `(mu4e-header-value-face            ((t (:foreground ,magenta))))
  `(mu4e-special-header-value-face    ((t (:foreground ,orange))))
  `(mu4e-contact-face                 ((t (:foreground ,green))))
  `(mu4e-compose-header-face          ((t (:foreground ,green :bold t))))
  `(mu4e-attach-number-face           ((t (:foreground ,cyan :bold t))))
  `(mu4e-footer-face                  ((t (:foreground ,cyan))))
  `(mu4e-compose-separator-face       ((t (:foreground ,grey))))
  `(mu4e-context-face                 ((t (:foreground ,blue :bold t))))
  `(mu4e-unread-face                  ((t (:foreground ,blue :bold t))))
  `(mu4e-forwarded-face               ((t (:foreground ,purple))))
  `(mu4e-draft-face                   ((t (:foreground ,magenta))))
  `(mu4e-replied-face                 ((t (:foreground ,green-bb))))
  `(mu4e-flagged-face                 ((t (:foreground ,orange))))
  `(mu4e-header-marks-face            ((t (:foreground ,cyan))))
  `(mu4e-region-code                  ((t (:foreground ,violet))))
  `(mu4e-link-face                    ((t (:inherit link))))
  `(mu4e-cited-1-face                 ((t (:foreground ,violet))))
  `(mu4e-cited-2-face                 ((t (:foreground ,purple))))
  `(mu4e-cited-3-face                 ((t (:foreground ,blue-d))))
  `(mu4e-cited-4-face                 ((t (:foreground ,grey))))
  `(mu4e-cited-5-face                 ((t (:foreground ,violet))))
  `(mu4e-cited-6-face                 ((t (:foreground ,purple))))
  `(mu4e-cited-7-face                 ((t (:foreground ,blue-b))))
  
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
