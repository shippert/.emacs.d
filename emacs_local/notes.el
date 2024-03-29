;
; My notes-mode descriptions.  TRS 8/27/02
;
; For emacs 22, the indent-relative doesn't work the way it used to; it
; will *sometimes* indent to col 3 if there is a symbol in col 1 of line 1,
; and sometimes it won't, depending, I think, on the symbol.  Thus, if the
; first line begins with * or !, it will indent the second line to match
; the first text column of the first, like it used to.  But if it begins
; with 1, 2, a, \, @, ? or some others, then it won't indent and line two
; begins at column 1.  Annoying.
;
; So this file defines hanging-indent-relative, which explicitly ignores
; whatever is in the first column by calling the (forward-char 1) inside
; the save-excursion.  Thus, it will always indent the next line to the
; first word break on or after column 2 of the first line.  This means we
; get some silliness if we start our line at column 1; i.e.:
;
; Monkeys eat bananas
;	and fruit
;	and throw poo
;
; I'm not so sure that's not what we want anyway, since we really want the
; first two columns to *always* be for tagging.  The line above should be:
;
;* Monkeys eat bananas
;  and fruit
;  and throw poo

; Also, there was a problem setting the keymap.  Apparantly, the
; text-mode-hook is inherited and called *last*, which means it will
; override anything we set before actually going into notes-mode.  This
; means: 
; (define-key notes-mode-map "\t" 'hanging-indent-relative)
;
; doesn't work, because we text-mode-hook resets it to 'tab-to-tab-stop
; once we go into notes-mode anyway.  What you have to do is set the define
; keys in an actual notes-mode-hook; fortunately, notes-mode-hook
; either is called after text-mode-hook or prevents it from being called
; altogether.  I'm sure this makes sense to somebody, but not to me.


(defface loud-face
   '((((class color)) (:foreground "black" :background  "orange")))
  "Loud, annoying face."
  :group 'faces)

(defvar loud-face 'loud-face
  "Variable to access loud-face face, so the symbol table works.")

(defface zoinks-face
   '((((class color)) (:foreground "black" :background  "greenyellow")))
  "Easy to see face."
  :group 'faces)

(defvar zoinks-face 'zoinks-face
  "Variable to access zoinks-face face, so the symbol table works.")

(defface red-face
  '((((class color)) (:background "red" :foreground "yellow")))
  "Another easy to see face."
  :group 'faces)

(defvar red-face 'red-face
  "Variable to access red-face face, so the symbol table works.")

(defface list-face
  '((((class color)) (:bold t :foreground "black" :background "cyan")))
  "For lists and stuff"
  :group 'faces)

(defvar list-face 'list-face "Shabado symbol table nonsense")
  
;; A mode for NOTES files - based on text-mode with just a couple changes
(defconst notes-mode-keywords
      (list
       ;;; Highlight the bullets
       '("^\\* " . font-lock-keyword-face)
       '("^\\(@\\)\\(X\\| \\)" 1 loud-face)
       '("^\\(!\\) " 1 zoinks-face)
       '("^\\(@\\)\\(X\\)" 2 zoinks-face)
       '("^\\(\\?\\) " 1 red-face)
       '("^\\([0-9\\|A-z]\\) " 1 list-face)
       ;; these are delimiter lines, for sections; Different colors
       ;; for the hell  of it
       '("^\\(=+\\)" 1 list-face)
       '("^\\(-+\\)" 1 zoinks-face)
       '("^\\(_+\\)" 1 red-face)
       ;; To cross out old or bad sections - delimit with a line of Xs 
       '("^\\(X+\\)" 1 red-face)   ;; Xs at the start
       '("\\(XXX+\\)" 1 red-face)  ;; 3 or more Xs anywhere
       ;; functions and vars
       '("\\b[\\\\.a-zA-Z0-9_-]+([^)]*?)" . font-lock-variable-name-face)
       '("\\b$[a-zA-Z0-9_-]+\\b" . font-lock-variable-name-face)
       ;;(list "^\\(@\\) " 1 'bold-italic)
       ;;; The dates that I tag with
       '("^[0-9]+/[0-9]+/[0-9]+" . font-lock-function-name-face)))


(defun hanging-indent-relative (&optional unindented-ok)
  "Just like indent-relative, only it works now with my notes mode.  Of
  course, it might not work in all ways.  The key thing is to forward-char
  1 in the save-excursion, which should ignore any initial characters in
  the first line when deciding when to indent to.  A work in progress."
  (interactive "P")
  (if (and abbrev-mode
	   (eq (char-syntax (preceding-char)) ?w))
      (expand-abbrev)
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "^[^\n]" nil t)
	  (let ((end (save-excursion (forward-line 1) (forward-char 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (or (looking-at "[ \t]")
		unindented-ok
		(skip-chars-forward "^ \t" end))
	    (skip-chars-forward " \t" end)
	    (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (move-marker opoint nil))
      (tab-to-tab-stop))))
)


;; Emacs 23 kludge
;; This really is stupid, but for some reason, indent-relative is what
;; I want once I put the point on column 0 after auto-filling.  The
;; save-excursion is for when I use it with the tab - it indents the
;; current line but leaves the point where it was.  This allows me to
;; format without having to jump to the front etc - mostly useful for
;; offset text and code snippets, but whatever

;; Actually, I kind of like this, and might use it as my default notes
;; mode from now on.
(defun stupid-indent-relative (&optional unindented-ok)
  (interactive "P")
  (save-excursion
    (move-to-column 0)
    (hanging-indent-relative)))

(define-derived-mode notes-mode
  text-mode "NOTES file"
  "Major mode for NOTES files (based on text-mode). \\{notes-mode-map}"
  (turn-on-auto-fill)
  ;; this allows the hanging tags
  ;;(setq indent-line-function 'hanging-indent-relative)
  (setq indent-line-function 'stupid-indent-relative)   ;; for emacs-23
  (setq fill-column 75)
  (abbrev-mode 1)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(notes-mode-keywords t))
  (font-lock-mode)
)

(add-hook 'notes-mode-hook
	  (function (lambda ()
		      (electric-indent-local-mode 0)
		      (define-key notes-mode-map "\t" 'indent-relative)
		      (define-key notes-mode-map "\e\t" 'tab-to-tab-stop))))


;  Trying to learn some font-locking.  Feh.
;  (setq font-lock-defaults '((list '("^*")) 't))
;  (font-lock-mode))

