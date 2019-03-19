(defun buffer-citation (&optional force)
  "Highlight cited text.
Each citation in the article will be highlighted with a different face.
The faces are taken from `gnus-cite-face-list'.
Attribution lines are highlighted with the same face as the
corresponding citation merged with `gnus-cite-attribution-face'.

Text is considered cited if at least `gnus-cite-minimum-match-count'
lines matches `gnus-cite-prefix-regexp' with the same prefix.

Lines matching `gnus-cite-attribution-suffix' and perhaps
`gnus-cite-attribution-prefix' are considered attribution lines."
  (interactive (list 'force))
  (save-excursion
;;    (set-buffer gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (alist gnus-cite-prefix-alist)
	  (faces gnus-cite-face-list)
	  (inhibit-point-motion-hooks t)
	  face entry prefix skip numbers number face-alist)
      ;; Loop through citation prefixes.
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      prefix (car entry)
	      numbers (cdr entry)
	      face (car faces)
	      faces (or (cdr faces) gnus-cite-face-list)
	      face-alist (cons (cons prefix face) face-alist))
	(while numbers
	  (setq number (car numbers)
		numbers (cdr numbers))
	  (and (not (assq number gnus-cite-attribution-alist))
	       (not (assq number gnus-cite-loose-attribution-alist))
	       (gnus-cite-add-face number prefix face))))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      prefix (cdr entry)
	      skip (gnus-cite-find-prefix number)
	      face (cdr (assoc prefix face-alist)))
	;; Add attribution button.
	(goto-line number)
	(when (re-search-forward gnus-cite-attribution-suffix
				 (save-excursion (end-of-line 1) (point))
				 t)
	  (gnus-article-add-button (match-beginning 1) (match-end 1)
				   'gnus-cite-toggle prefix))
	;; Highlight attribution line.
	(gnus-cite-add-face number skip face)
	(gnus-cite-add-face number skip gnus-cite-attribution-face))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-loose-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      skip (gnus-cite-find-prefix number))
	(gnus-cite-add-face number skip gnus-cite-attribution-face)))))
