;
; This is a mode for reading a DOD, or ncdump.  It's based off of notes.el,
; and it's mainly just font-locking, rather than things like hanging
; indents or what not.  Although that would be cool.

(defface loud-face
   '((((class color)) (:foreground  "orange")))
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
  '((((class color)) (:bold t :foreground "red")))
  "Another easy to see face."
  :group 'faces)

(defvar red-face 'red-face
  "Variable to access red-face face, so the symbol table works.")

(defface number-face
  '((((class color)) (:foreground "RoyalBlue3")))
  "Another easy to see face."
  :group 'faces)

(defvar number-face 'number-face
  "Variable to access number-face face, so the symbol table works.")

(defface list-face
  '((((class color)) (:bold t :background "cyan")))
  "For lists and stuff"
  :group 'faces)

(defvar list-face 'list-face "Shabado symbol table nonsense")
  
;; A mode for NOTES files - based on text-mode with just a couple changes
(defconst dod-mode-keywords
      (list
       ;; Strings first, because anything can be in our attributes and we
       ;; don't want them to match anything else
       '("\"\\(.*\\)\"" 1 font-lock-string-face)
       ;; First, bring out the datastream name up top.  Silly, but why not
       '("^netcdf \\([^{]*\\)" 1 font-lock-keyword-face)
       ;; Field names and dimensions, each different.  This is serious
       ;; font-lock stuff now, boys and girls 
       '("\\b\\([a-zA-Z0-9_-]+\\)(\\([^)]*?\\))"
	  (1 font-lock-function-name-face)
	  (2 font-lock-variable-name-face))
       ;; For 0-dim fields - need to have the type up front, too, so this
       ;; is long
       '("\\(int\\|float\\|double\\) \\([a-zA-Z0-9_-]+\\) ;"
	 2 font-lock-function-name-face)
       ;; Actually, numbers too - in comment face?, too
       '("= \\([-0-9][0-9\.+ef]*\\)" 1 number-face)
       ;; Now the attribute names
       ;;'("\\b[a-zA-Z0-9_-]+:\\([^=]+\\)" 1 font-lock-reference-face)
       ;;'(":\\([^=]+\\) =" 1 font-lock-reference-face)
       ;;'("\\b\\([^=]+\\)\:" 1 font-lock-reference-face)
       '("\\([a-zA-Z0-9_-]+\\):" 1 font-lock-reference-face)
       ))

(define-derived-mode dod-mode
  text-mode "DOD file"
  "Major mode for DOD files (based on text-mode). \\{dod-mode-map}"
  ;; this allows the hanging tags
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dod-mode-keywords t))
  (font-lock-mode)
)


;  Trying to learn some font-locking.  Feh.
;  (setq font-lock-defaults '((list '("^*")) 't))
;  (font-lock-mode))

