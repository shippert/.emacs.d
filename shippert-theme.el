(deftheme shippert
  "Created 2017-05-03.")

(custom-theme-set-variables
 'shippert
 '(custom-safe-themes (quote ("49a3c59e4b1ca3d1b2e4e19fbc41fa93e7f8613ff3d92010d90027125f1fe6da" default)))
 '(load-home-init-file t))

(custom-theme-set-faces
 'shippert
 ;; basic theming.
 '(font-lock-comment-face ((t (:foreground "blue3" :italic t))))
 '(font-lock-keyword-face ((t (:foreground "red3" :bold t))))
 '(font-lock-string-face ((t (:foreground "green4" ))))
 '(font-lock-type-face ((t (:foreground "orangered" :bold t :italic t))))
 '(font-lock-function-name-face ((t (:foreground "blue4" :bold t :italic t))))
 '(font-lock-variable-name-face ((t (:foreground "blue3" ))))
 '(font-lock-reference-name-face ((t (:foreground "blue3" ))))
 '(scroll-bar ((t (:background "lightblue" ))))
 '(menu ((t (:foreground "black" :background "lightblue" ))))
)

(provide-theme 'shippert)
