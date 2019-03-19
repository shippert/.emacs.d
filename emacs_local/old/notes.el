;
; My notes-mode descriptions.  TRS 8/27/02
;

(defface loud-face
   '((((class color)) (:background  "orange")))
  "Loud, annoying face."
  :group 'faces)

(defvar loud-face 'loud-face
  "Variable to access loud-face face, so the symbol table works.")

(defface zoinks-face
   '((((class color)) (:background  "greenyellow")))
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
  '((((class color)) (:bold t :background "cyan")))
  "For lists and stuff"
  :group 'faces)

(defvar list-face 'list-face "Shabado symbol table nonsense")

;; A mode for NOTES files - based on text-mode with just a couple changes
(defconst notes-mode-keywords
      (list
       ;;; Highlight the bullets
       '("^\\* " . font-lock-keyword-face)
       '("^\\(@\\)\\(X\\| \\)" 1 loud-face)
       '("^\\(\\!\\) " 1 zoinks-face)
       '("^\\(@\\)\\(X\\)" 2 zoinks-face)
       '("^\\(\\?\\) " 1 red-face)
       '("^\\([0-9\\|A-z]\\) " 1 list-face)
       ;; these are underlines, for sections; Different colors for 
       ;; the hell of it
       '("^\\(=+\\)" 1 list-face)
       '("^\\(-+\\)" 1 zoinks-face)
       '("^\\(_+\\)" 1 red-face)
       '("^\\(X+\\)" 1 red-face)
       ;; functions and vars
       '("\\b[a-zA-Z0-9_-]+([^)]*?)" . font-lock-variable-name-face)
       '("\\b$[a-zA-Z0-9_-]+\\b" . font-lock-variable-name-face)
       ;;(list "^\\(@\\) " 1 'bold-italic)
       ;;; The dates that I tag with
       '("^[0-9]+/[0-9]+/[0-9]+" . font-lock-function-name-face)))

(define-derived-mode notes-mode
  text-mode "NOTES file"
  "Major mode for NOTES files (based on text-mode). \\{notes-mode-map}"
  (turn-on-auto-fill)
  ;; this allows the hanging tags
  (setq indent-line-function 'indent-relative)  
  (setq fill-column 75)
  (abbrev-mode 1)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(notes-mode-keywords t))
  (font-lock-mode)
  )

(add-hook 'notes-mode-hook
	  (function (lambda ()
	    (define-key notes-mode-map "\t" 'indent-relative)
	    (define-key notes-mode-map "\e\t" 'tab-to-tab-stop))))

;  Trying to learn some font-locking.  Feh.
;  (setq font-lock-defaults '((list '("^*")) 't))
;  (font-lock-mode))


