(deftheme fountain
  "A theme for scriptwriting.")

(custom-theme-set-faces
 'fountain
 '(fountain ((t (:height 120 :family "Courier Prime"))))
 '(fountain-note
   ((((background light)) (:background "#eee7ad"))
    (((background dark)) (:foreground "goldenrod"))))
 '(fountain-synopsis
   ((((background light)) (:background "light cyan" :foreground "grey50"))
    (((background dark)) (:foreground "dark cyan"))))
 '(fountain-section-heading ((t (:weight bold :inherit variable-pitch))))
 '(fountain-scene-heading ((t (:underline t)))))

(provide-theme 'fountain)
