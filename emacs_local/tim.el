;(defun do-auto-fill ()
;  (let ((fill-point
;	 (let ((opoint (point)))
;	   (save-excursion
;	     (move-to-column (1+ fill-column))
;	     (skip-chars-backward "^ \t\n")
;	     (if (bolp)
;		 (re-search-forward "[ \t]" opoint t))
;	     (skip-chars-backward " \t")
;	     (point)))))
;    ;; If there is a space on the line before fill-point,
;    ;; and nonspaces precede it, break the line there.
;    (if (save-excursion
;	  (goto-char fill-point)
;	  (not (bolp)))
;	;; If point is at the fill-point, do not `save-excursion'.
;	;; Otherwise, if a comment prefix or fill-prefix is inserted,
;	;; point will end up before it rather than after it.
;	(if (save-excursion
;	      (skip-chars-backward " \t")
;	      (= (point) fill-point))
;;	    (indent-new-comment-line)
;	    (newline-and-indent)
;	    (save-excursion
;	      (goto-char fill-point)
;;	      (indent-new-comment-line)
;	      (newline-and-indent))))))

(defun indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment."
  (interactive "*")
  (beginning-of-line 1)
  (if (null comment-start)
      (error "No comment syntax defined")
    (let* ((eolpos (save-excursion (end-of-line) (point)))
	   cpos indent begpos)
      (if (re-search-forward comment-start-skip eolpos 'move)
	  (progn (setq cpos (point-marker))
		 (goto-char (match-beginning 0))))
      (setq begpos (point))
      (setq indent (funcall comment-indent-function))
      (skip-chars-backward " \t")
      (delete-region (point) begpos)
      (indent-to indent)
      (if cpos 
	  (progn (goto-char cpos)
		 (set-marker cpos nil))
	(insert comment-start)
	(save-excursion
	  (insert comment-end)))
      (indent-according-to-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST JUNQUE

(defun highlight-region (string)
  "Inserts STRING at the beginning of every line in the region.
Called interactively, STRING defaults to comment-start (or '>> ' if
none is defined) unless a prefix argument is given, in which case it
prompts for a string."
  (interactive (list (if current-prefix-arg
                         (read-string "String to insert: ")
                       (or comment-start
                           ">> "))))
  (save-excursion
    (if (bolp)
        (forward-line -1))
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
        (replace-match string))
      )))


;I find this a little more elegant (no narrowing or regular expression
;stuff):


(defun my-indent-region-with-string (indent-string)
  "Prefixs every line in the region with STRING."
  (interactive "*sIndent region with string: ")
  (if (equal indent-string "")
      (setq indent-string "> "))
  (save-excursion
    (let ((reglines    (count-lines (region-beginning) (region-end)))
          (line        1))
      (goto-char (region-beginning))
      (while (<= line reglines)
        (beginning-of-line)
        (insert indent-string)
        (setq line (+ line 1))
        (forward-line 1)))))
