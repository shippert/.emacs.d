(defun indent-relative-tab()
  " "
  (interactive)
  (indent-relative-hanging-tab t))

(defun indent-relative-hanging-tab (&optional unindented-ok)
  "My attempt to make a text mode indenter that will ignore the
leading tab in a paragraph"
  (interactive "P")
  (if (and abbrev-mode
	   (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
;;
      (and (re-search-backward "^[^\n]" nil t)
	   (save-excursion
	     (let ((foo (save-excursion
			  (forward-line -2)
			  (point))))
	       (forward-char 1)
	       (re-search-backward "\n\n\t" foo t)))
	   (let ((end (save-excursion (forward-line 1) (point))))
	     (move-to-column start-column)
	     ;; Is start-column inside a tab on this line?
	     (if (> (current-column) start-column)
		 (backward-char 1))
	     (or (looking-at "[ ]")
		 unindented-ok
		 (skip-chars-forward "^ \t" end))
	     (skip-chars-forward " " end)
	     (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (move-marker opoint nil)))))
