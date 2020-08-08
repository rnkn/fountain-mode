(deftheme fountain
  "A theme for scriptwriting.")

(custom-theme-set-faces
 'fountain
 '(fountain ((t (:height 1.2 :family "Courier Prime"))))
 '(fountain-note
   ((((background light)) (:background "#eee7ad"))
    (((background dark)) (:foreground "goldenrod"))))
 '(fountain-synopsis
   ((((background light)) (:background "light cyan" :foreground "grey50"))
    (((background dark)) (:foreground "dark cyan"))))
 '(fountain-section-heading-1 ((t (:height 1.1 :inherit fountain-section-heading-2))))
 '(fountain-section-heading-2 ((t (:height 1.1 :inherit fountain-section-heading-3))))
 '(fountain-section-heading-3 ((t (:height 1.1 :inherit fountain-section-heading-4))))
 '(fountain-section-heading-4 ((t (:inherit fountain-section-heading-5))))
 '(fountain-section-heading-5 ((t (:weight bold :inherit variable-pitch))))
 '(fountain-scene-heading ((t (:underline t)))))

(provide-theme 'fountain)
