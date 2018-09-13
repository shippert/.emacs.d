(if (not (string-match emacs-version "21.4.1"))
    (set-default-font "Liberation Mono-12"))

; Set up for Sparc (now), customized for my comfort, and this is just
(define-key global-map "\C-l" 'forward-char)
(define-key global-map "\C-f" 'recenter)
(define-key ctl-x-map "\C-r" 'replace-string)
;;(define-key ctl-x-map "r" 'rmail)
(define-key ctl-x-map "\C-q" 'query-replace)
;; ESC sequence is \C-[
(define-key esc-map "g" 'goto-line)
(define-key esc-map "$" 'ispell-word)
(define-key esc-map "w" 'ispell-complete-word)
(define-key esc-map "\t" 'self-insert-command)
(define-key ctl-x-map "y" 'yow)
(define-key ctl-x-map "c" 'compile)
(define-key ctl-x-map "p" 'tprint-buffer) ; in tprint.el
(define-key ctl-x-map "f" 'flame)
;;(define-key ctl-x-map "r" 'gnus)
(define-key ctl-x-map "m" 'message-mail)
(define-key ctl-x-4-map "f" 'font-lock-fontify-buffer)
(define-key ctl-x-map "l" 'add-change-log-entry)
(global-unset-key "\C-]")
(define-key ctl-x-4-map "x" 'exchange-point-and-mark)
(define-key ctl-x-4-map "h" 'split-window-horizontally)
(define-key ctl-x-4-map "a" 'auto-fill-mode)
(define-key ctl-x-4-map "k" 'copy-region-as-kill)
(define-key ctl-x-4-map "d" 'delete-rectangle)
(define-key ctl-x-map "vq" 'vc-toggle-read-only)
(define-key ctl-x-map "\C-d" 'date)

; The "R2" key on a sun keypad is PrSc
; This isn't as cool as I'd like, because it doesn't 2-up
(define-key global-map [f12] 'ps-print-buffer-with-faces)
(define-key global-map [print] 'ps-print-buffer-with-faces)

; Get the keypad right on wombat
(define-key global-map [f35] 'scroll-up)
(define-key global-map [f29] 'scroll-down)

(if (string-match emacs-version "21.4.1")
    (standard-display-european t))

(turn-on-auto-fill)

;(require 'diff-mode-.el)

; for TM
;(setenv "TM_TMP_DIR" "/home/d3a230/mime")

;; I'm not supposed to do this, but I don't like dark blue
;; Has to be in .emacs, before we run gnus down there with the TM stuff
(setq gnus-face-dark-name-list
  (list "blue3" "firebrick" "dark green" "OrangeRed" "dark khaki"
	 "dark violet" "SteelBlue4")) 

; ;;; set my faces
(setq font-lock-maximum-decoration t)

;
(setq-default case-fold-search nil)
(setq require-final-newline 'foo)
(setq rmail-file-name "~/mail/RMAIL")
(setq rmail-last-rmail-file "~/mail/XMAIL")
(setq rmail-default-rmail-file "~/mail/XMAIL")
(setq rmail-secondary-file-directory "~/mail")
(setq rmail-in-reply-to-use-message-id nil)
;(setq rmail-dont-reply-to-names
;      "info-\\|tr_shippert\\|d3a230\\|timothy.shippert\\>")
(setq rmail-dont-reply-to-names
      "info-\\|\"?Shippert\\(, Tim\\(othy\\)?\\( R\"?\\)?\\)?\\|tr_shippert\\|tim\\(othy\\)?.shippert\\|Tim\\(othy\\)? Shippert\\|d3a230")
;

;(setq compile-command "/svr/apps/gnu_utils/bin/make")

(setq browse-url-netscape-program "/usr/bin/mozilla")

;; set my faces
;; New face here for green - so I can keep underlining.
(make-face 'emphasize)

;; For emacs version 23+, we can use themes to change colors and stuff
;; Version 24 themes will skip confirmation with a following true, but
;; 23 does not.
(cond ((>= emacs-major-version 24) (load-theme 'gruber-darker t))
      ((= emacs-major-version 23) (load-theme 'gruber-darker))
      (t 
       ;; Prior to themes, do it the hard way
	(progn
	  (setq font-lock-comment-face 'italic)
	  (setq font-lock-keyword-face 'bold)
	  (setq font-lock-string-face 'emphasize)
	  (setq font-lock-type-face 'bold-italic)
	  (setq font-lock-function-name-face 'bold-italic)
	  
	  (set-face-foreground 'emphasize "green4")
	  (set-face-foreground 'italic "blue3")
	  (set-face-foreground 'bold "red3")
	  (set-face-foreground 'underline "firebrick")
	  (set-face-foreground 'bold-italic "blue4")
	  (make-face-bold 'bold-italic nil t)
	  
	  ;; only need this here, because italic shows up bold on squid
	  (make-face-unbold 'italic nil t)
	  
	  (setq font-lock-face-attributes  
		(list '(font-lock-comment-face "blue3")
		      '(font-lock-string-face "green4")
		      '(font-lock-keyword-face "darkgoldenrod" nil t)
		      '(font-lock-function-name-face "red3" nil t) 
		      '(font-lock-variable-name-face "blue3" nil t) 
		      '(font-lock-type-face "orangered") 
		      '(font-lock-reference-face "blue3" nil t))))))
      
; Awful way to do this, and slime sucks anyway.  Keeping it here for reference.
;(if (>= emacs-major-version 24)
;    ;;
;    (load-library (expand-file-name "~/emacs/slime-theme.el")))

(global-font-lock-mode t)

(setq load-path
 (append (list (expand-file-name "~/emacs")
	       (expand-file-name "/usr/local/lib/emacs/site-lisp")
	       )
	 load-path))

; to test the latest gnus
;(setq load-path (cons (expand-file-name "/usr/local/src/gnus-5.10.6/lisp") load-path))
;(require 'gnus-load)
;(setq load-path (cons (expand-file-name "/usr/local/src/emacs-21.3/lisp/gnus") load-path))

; This stuff apparantly won't work with my podunk emacs...
;(setq load-path (cons "/usr/local/src/custom-1.9962"
;		      (cons "/usr/local/src/gnus-5.6.24/lisp"
;			    load-path)))
;(setq load-path (cons "/usr/local/src/gnus-5.8.3/lisp" load-path))

;(setq Info-default-directory-list 
;        (cons "/usr/local/src/gnus-5.6.24/texi" Info-default-directory-list))

;(setq Info-default-directory-list 
;        (cons "/usr/local/src/gnus-5.8.3/texi" Info-default-directory-list))

(if window-system
    (require 'igor-mode))

(require 'generic)

(define-generic-mode 'flat-generic-mode
  (list ?# )
  '()
  (list
   '("^[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*:" 1 'font-lock-type-face)
   '("[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*:" 1 'font-lock-constant-face)
   '("[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*=" 1 'font-lock-variable-name-face)
   )
  (list "\\.flat")
  (list 'font-lock-mode 'auto-fill-mode))

(define-generic-mode 'vip-generic-mode
  (list ?# ?\;)
  '()
  (list
   '("[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*=" 1 'font-lock-function-name-face)
   )
  (list "vip.*\\.txt")
  (list 'font-lock-mode 'auto-fill-mode))


; Print setup
(setq tprint-command "tpr")
(setq tprint-switches " ")
(setq tprint-printer (getenv "PRINTER"))

; Rmail print setup
(setq mprint-command "tpr")
(setq mprint-switches "-F")
(setq mprint-printer (getenv "PRINTER"))

; Rmail print setup
(setq rmail-print-command
      "cpr")
(setq rmail-print-switches
      "-l -F")
(setq rmail-print-printer tprint-printer)

; RMAIL output defaults
(setq rmail-output-file-alist
      '(
	("cart@met.das.bnl.gov" . "SITEOPS")
	("opslog@met.das.bnl.gov" . "SITEOPS")
	("mdslog@" . "SITEOPS")
	("opslog@" . "SITEOPS")))

;; NOTES mode autoload - load notes.el
(autoload 'notes-mode	   "notes" "Notes mode" t)
(autoload 'dod-mode	   "dod" "DOD mode" t)

; Rmail faces - I don't like this, but I need to make quoted stuff
; use the comment face, not the reference face.
(defvar rmail-font-lock-keywords 
  (eval-when-compile 
    (let* ((cite-prefix "A-Za-z") (cite-suffix (concat cite-prefix "0-9_.@-")))
      (list
       '("^\\(From\\|Sender\\):" . font-lock-function-name-face)
       '("^Reply-To:.*$" . font-lock-function-name-face)
       '("^Subject:.*$" . font-lock-reference-face)
       '("^Subject:" . font-lock-comment-face)
       '("^\\(To\\|Apparently-To\\|Cc\\):" . font-lock-keyword-face)
       (cons (concat "^[ \t]*"
		     "\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
		     "[>|}].*")
	     'font-lock-comment-face)
       '("^\\(X-[A-Za-z0-9-]+\\|In-reply-to\\|Date\\):.*$"
	 . font-lock-string-face))))
  "Additional expressions to highlight in Rmail mode.")

; Misc setup
;;(setq mail-archive-blind-file "~/mail/archive")
(setq mail-archive-blind-file nil)
(setq rmail-in-reply-to-use-message-id nil)
(setq comment-column -5)
(setq lpr-command "lp")
(setq lpr-switches '("-2l" "-ln1"))
(setq ps-lpr-command "lp")
(setq ps-lpr-switches nil)
(setq fortran-continuation-string "+")

(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      (quote	       
		(".lpt" ".PS" ".aux"  ".bak"))))

(setq default-major-mode (quote text-mode))

;; pod mode
(require 'pod-mode)

(setq auto-mode-alist
      (append '(("\\.letter$" . mail-mode))
	      '(("Makefile.*$" . makefile-mode))
	      '(("\\.gnus$" . emacs-lisp-mode))
	      '(("\\.pro$" . idlwave-mode))
	      '(("\\.F90$" . f90-mode))
	      '(("NOTES\\..*" . notes-mode))
	      '(("\\.dod$" . dod-mode))
	      '(("\\.cdl$" . dod-mode))
	      '(("README\\..*" . text-mode))
	      '(("vip.*\\.txt" . vip-generic-mode))
	      '(("\\.txt" . text-mode))
	      '(("\\.dat" . text-mode))
	      '(("\\.ipf" . igor-mode))
	      '(("\\.pod$" . pod-mode))
	      auto-mode-alist))

;(setq auto-mode-alist (append auto-mode-alist
;			      (quote (("\\.roff$" . nroff-mode)))))

(add-hook 'perl-mode 'turn-on-font-lock)

(add-hook 'pod-mode-hook 'font-lock-mode)
(add-hook 'pod-mode-hook 'turn-on-auto-fill)

(add-hook 'sh-mode-hook 'turn-on-auto-fill)

(add-hook 'text-mode-hook
	  (function (lambda ()
		      (turn-on-auto-fill)
		      (local-set-key "\t" 'tab-to-tab-stop)
		      (local-set-key "\e\t" 'indent-relative)
		      ;; I think adaptive-fill-mode is hopelessly
		      ;; munged for text mode; Better to just turn it
		      ;; off altogether, and then make my own
		      ;; indent-line-function that ignore hanging tabs
		      (make-local-variable 'adaptive-fill-mode)
		      (setq adaptive-fill-mode nil)
		      ;(setq indent-line-function 'indent-relative)
		      (setq fill-column 75)
		      (abbrev-mode 1))))

(add-hook 'html-helper-mode-hook
	  (function (lambda ()
		      ;; list indent doesn't work right in html-helper
		      (setq html-helper-item-continue-indent 0))))

(add-hook 'c-mode-hook
	  (function (lambda ()
		      (turn-on-auto-fill)
		      (setq fill-column 75)
		      (setq comment-column 0)
		      (local-set-key "\e\t" 'tab-to-tab-stop)
		      (setq c-electric-flag 't)
		      (line-number-mode 't)
		;      (font-lock-mode)
		      )))

(add-hook 'perl-mode-hook
	  (function (lambda ()
		      (turn-on-auto-fill)
		      (setq fill-column 75)
		      (setq perl-indent-level 2))))


(add-hook 'fortran-mode-hook
	  (function (lambda () 
		      (turn-on-auto-fill)
		      (setq fill-column 72)
		      (setq comment-column 0)
		      )))

(add-hook 'rmail-summary-mode-hook
	  (function
	   (lambda ()
	     (turn-on-font-lock)
	     (local-set-key "D"
			    'rmail-summary-delete-forward-nokidding)
	     )))

(add-hook 'rmail-mode-hook                   ;;; Stupid stupid stupid
      (function
       (lambda ()
	 (local-set-key "e" 'rmail-edit-current-message)
	 (local-set-key "x" 'rmail-expunge)
	 (local-set-key "P" 'mprint-buffer)
	 (local-set-key "\C-xp" 'mprint-buffer)
	 (turn-on-font-lock)
	 (setq rmail-highlighted-headers "^From:" )
	 (setq rmail-ignored-headers
	       (concat
		rmail-ignored-headers
;		"\\|^Mime-Version:\\|^Content-Type:\\|^X-Sender:"))
		"\\|^X-Sender:"))
	 )))

;; goofy rmime.el stuff
;(if (or (equal "vt200" (getenv "TERM" ))
;	(equal "vt220" (getenv "TERM" )))
;    nil
;  (add-hook 'rmail-show-message-hook 'rmime-format)
;  (add-hook 'rmail-edit-mode-hook 'rmime-cancel)
;(setq rmail-output-file-alist '(("" rmime-cancel)))
;  (autoload 'rmime-format "rmime" "" nil))

; Try tm for mime stuff for now - see below.

(setq mail-mode-hook
      '(lambda nil
         (progn
;          (local-set-key "\eq" 'fill-quoted-paragraph)
           (local-set-key "\C-c\C-y" 'mail-yank-tim)
           (local-set-key "\C-xa" 'mail-realias)
	   (setq mail-default-reply-to "timothy.shippert@pnl.gov")
           (setq mail-yank-prefix ">")
           (setq mail-header-separator
                 "-------")
	   (setq mime-editor/split-message nil) ; this is important for tm!
           (setq mail-yank-ignored-headers
                 (concat mail-yank-ignored-headers
                         "\\|^X-Sender:"))
	   (turn-on-font-lock)
	   ;; reset mail-citation-hook so we can do RMAIL and GNUS
	   ;; mail.  What a pain.
	   (setq mail-citation-hook
		 '(lambda nil
		    (progn
		      (mail-fixup-citation))))
           )))

(add-hook 'message-mode-hook
	  (function
	   (lambda ()
	     (setq mail-header-separator "-------")
	     ;; we have to actually _reset_ mail-citation-hook,
	     ;; because message-yank-original calls it, and we want
	     ;; GNUS to do that wacky citation stuff.  If we reset it
	     ;; down here in the hook, then it still works for RMAIL.
	     (local-set-key "\C-xa" 'mail-realias)
	     (setq mail-citation-hook nil) 
	     (setq message-yank-prefix ">")
	     (setq message-cite-function
		   'message-cite-original-without-signature)
	     (set-face-foreground 'message-cited-text-face
				  "firebrick")
	     (set-face-foreground 'message-header-to-face "red4")
	     (set-face-foreground 'message-header-name-face "green4")
	     ;; I don't like how it auto-fills with a tab when I start
	     ;; a paragraph with a tab. Especially since it doesn't
	     ;; seem to work with other regexps.
	     (make-local-variable 'adaptive-fill-mode)
	     (setq adaptive-fill-mode nil)
	     (setq normal-auto-fill-function 'do-auto-fill) ;; for
	     ;; gnus 5.10.6?
	     ;; But, I do like adaptive-fill-mode on some things, like
	     ;; message-newline-and-reformat.  C'est la vie.
	     (setq message-default-headers "Reply-To: tim.shippert@pnl.gov")
	     (local-set-key "\M-\r"
			    'message-newline-and-adaptive-reformat)
	     (local-set-key "\C-c\C-x\t" 'mime-editor/insert-file) 
	     (setq mime-editor/split-message nil) ; this is important for tm!
	     (setq message-signature nil)
	     )))

(defun generic-send-hook ();;;mail-send-hook
  (progn
    (interactive)
    (if (y-or-n-p "Insert ~/.signature file?")
	(mail-signature nil))
    (if mail-archive-blind-file
	(progn
	  (mail-position-on-field "Subject")
	  (insert "\nFCC: " mail-archive-blind-file)))))

(add-hook 'message-send-hook 'generic-send-hook)
(add-hook 'mail-send-hook 'generic-send-hook)
	  
(add-hook 'gnus-summary-mode-hook
	  '(lambda nil
	     (progn
	       (local-set-key [print] 'gnus-summary-print-article)
	       (setq mm-default-directory "~/MIME/"))))

(add-hook 'idl-mode-hook
	  (function 
	   (lambda ()
	     (setq			; Set options here
	      idl-block-indent 2          ; Indentation settings
	      idl-main-block-indent 2
	      idl-end-offset -2
	      idl-continuation-indent 2
	      idl-begin-line-comment "^\;[^\;]" 
	      idl-surround-by-blank nil   ; Turn on padding symbols =,<,>, etc.
	      abbrev-mode 1	; Turn on abbrevs (-1 for off)
	      idl-pad-keyword nil ; Remove spaces for keyword assign '='
	      ;; If abbrev-mode is off, then case changes (the next 2 lines)
	      ;; will not occur.
	      idl-reserved-word-upcase t  ; Change reserved words to upper case
	      idl-abbrev-change-case nil  ; Don't force case of expansions
	      ; idl-hang-indent-regexp ": " ; Change from "- "
	      idl-show-block t          ; Turn off blinking to matching begin
	      idl-abbrev-move t        ; Allow abbrevs to move point backwards
	      case-fold-search nil   ; Make searches case sensitive
	      )
	     (turn-on-font-lock)
	     )))

(setq emacs-lisp-mode-hook '(lambda nil (progn
				  (turn-on-auto-fill)
				  ;(setq fill-column 75)
				  (setq comment-column 0)
				;  (font-lock-mode)
				  )))

(setq scribe-mode-hook '(lambda nil (progn
				    (turn-on-auto-fill)
				   ; (setq fill-column 108)
				    (read-abbrev-file 
				     (expand-file-name ".abbrev_file"))
				    (abbrev-mode 1))))

(setq server-switch-hook
      '(lambda nil
	 (progn
	   (local-set-key "\C-c\C-c" 'server-edit))))

(if (equal "vt220" (getenv "TERM" ))
    nil
  (add-hook 'dired-mode-hook 'turn-on-font-lock)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
  (add-hook 'c-mode-hook 'turn-on-font-lock)
  (add-hook 'fortran-mode-hook 'turn-on-font-lock)
  )


;; A generic mode for info files.  Let's see how it goes
(require 'generic)

(define-generic-mode 'flat-generic-mode
  (list ?# )
  '()
  (list
   '("^[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*:" 1 'font-lock-type-face)
   '("[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*:" 1 'font-lock-constant-face)
   '("[ \t]*\\([a-zA-Z0-9_-]+\\)[ \t]*=" 1 'font-lock-variable-name-face)
   )
  (list "\\.flat")
  (list 'font-lock-mode 'auto-fill-mode))

(define-generic-mode 'infofile-generic-mode
  (list ?# )
  '()
;  (list "float" "double" "int" "long_int" "long[^_]?" "char")
  (list "^[ \t]*sgp[^ \t]*"
	"^[ \t]*twp[^ \t]*"
	"^[ \t]*nsa[^ \t]*"
	"^[ \t]*gec[^ \t]*"
	"^[ \t]*pye[^ \t]*"
	"^[ \t]*nim[^ \t]*"
	"^[ \t]*NULL[^ \t]*"
	;; Not sure what to do about "long_name" and the "long" type
	'("\\b\\([a-zA-Z0-9_-]+\\)(\\([^)]*?\\))"
	  (1 font-lock-function-name-face)
	  (2 font-lock-variable-name-face))
	'("\\b$[a-zA-Z0-9_-]+\\b" . font-lock-variable-name-face)
	'("{\\([^}]*\\)}" 1 font-lock-reference-face)
        '("\\b\\(float\\|double\\|int\\|long_int\\|\\|char\\)\\b" .
	  font-lock-reference-face)
	'("= \\(.*\\)" 1 font-lock-string-face)
	)
  (list "\.info")
  (list 'font-lock-mode))

;;;
;(autoload 'rmail-tim-setup "rmail-tim" "My redefs of some things" t)
;(autoload 'mail "mail-tim" "Load up mail file" t)

;(autoload 'rmail-execute-content-type' "mm" "MIME stuff - wacky and
;old, but it works" t)
(autoload 'indent-relative-tab "indent-tim" "My attempt to fix indent-relative" t)

(autoload 'buffer-citation "buffer-cite" "Highlight citations in
current buffer (used for printing email)" t)

(autoload 'tprint-buffer' "tprint" "My customization of printing
 stuff" t)

(autoload 'mprint-buffer' "mprint" "My customization of mail printing stuff
 - there's got to be a better way" t)

(autoload 'ispell "ispell" "Run ispell over buffer" t)
(autoload 'ispell-region "ispell" "Run ispell over region" t)
(autoload 'ispell-word "ispell" "Check word under cursor" t)

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))

;(autoload 'calendar "cal" 
;  "\
;Load the damn calendar file"
; t)

(autoload 'vpr-buffer "vpr"
  "\
print out buffer using vpr"
 t)

(autoload 'idlwave-mode "idlwave" "IDLWAVE Mode" t)
(autoload 'idlwave-shell "idlw-shell" "IDLWAVE Shell" t)


;(setq 'idlwave-surround '())

(cond (window-system
       (mwheel-install)))

;;; You might want to bind ispell-word and ispell-complete word to
;;; keys.


;;; Ess stuff
(load "/home/shippert/emacs/ess-5.3.7/lisp/ess-site.el")

;;;
;;;(load "sendmail")
(load "tim")

;
; Here it is - the tm stuff to read MIME with rmail.
;
;; I think if I load gnus first then mime-setup, the mouse buttons
;; will work right in both.  I don't know why.
;;(load "gnus")
;;(load "mime-setup")

;(load "next-line")

;;; Terminal stuff
;(let ((termoo (getenv "TERM")))
;  (cond ((equal termoo "vt102")   ; modem
;	 (load "~/.emacs.vt102"))
;	)
;  )

(display-time)

;;; Commands added by calc-private-autoloads on Mon Jun 29 11:31:53 1998.
(autoload 'calc-dispatch	   "calc" "Calculator Options" t)
(autoload 'full-calc		   "calc" "Full-screen Calculator" t)
(autoload 'full-calc-keypad	   "calc" "Full-screen X Calculator" t)
(autoload 'calc-eval		   "calc" "Use Calculator from Lisp")
(autoload 'defmath		   "calc" nil t t)
(autoload 'calc			   "calc" "Calculator Mode" t)
(autoload 'quick-calc		   "calc" "Quick Calculator" t)
(autoload 'calc-keypad		   "calc" "X windows Calculator" t)
(autoload 'calc-embedded	   "calc" "Use Calc inside any buffer" t)
(autoload 'calc-embedded-activate  "calc" "Activate =>'s in buffer" t)
(autoload 'calc-grab-region	   "calc" "Grab region of Calc data" t)
(autoload 'calc-grab-rectangle	   "calc" "Grab rectangle of data" t)
(setq load-path (nconc load-path (list "/usr/local/src/calc-2.02f")))
(global-set-key "\e#" 'calc-dispatch)
(global-set-key [SunF36] 'calc-dispatch)

;;; End of Calc autoloads.

;;; Custom mail stuff.

(defun mail-realias ()
  "Rebuild the alias list from .mailrc"
  (interactive)
  (princ "Loading ~/.mailrc...")
  (build-mail-aliases)
  (princ "done")
)

; re-written to do what _I_ want.  I defun it here so that it
; supercedes all other defuns later on...

(defun mail-fixup-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  (let ((start (point)))
    (mail-yank-clear-headers start (mark t))
    (if (null mail-yank-prefix)
        (indent-rigidly start (mark t) mail-indentation-spaces)
      (save-excursion
        (goto-char start)
        (while (< (point) (mark t))
          (if (not (looking-at "^[ \t]*$")) ; don't quote whitespace
              (insert mail-yank-prefix))
          (forward-line 1))))))

(defun mail-yank-tim (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Normally, indents each nonblank line ARG spaces (default 3).
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line.

Just \\[universal-argument] as argument means don't indent, insert no prefix,
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
        (delete-windows-on mail-reply-buffer)
        (insert-buffer mail-reply-buffer)
        (if (consp arg)
            nil
          (goto-char start)
          (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
                                           mail-indentation-spaces)))
            (if mail-citation-hook
                (run-hooks 'mail-citation-hook)
              (run-hooks 'mail-yank-hooks))))
;       (exchange-point-and-mark)
        )))


(defun rmail-summary-delete-forward-nokidding (arg)
  "Silly hack so we can delete ARG number of messages"
  (interactive "p")
  (let ((foo (if (null arg) 1 arg))
	(num 0))
    (while (< num foo)
      (rmail-summary-delete-forward)
      (setq num (1+ num)))))

(defun message-newline-and-adaptive-reformat()
  "Do the message-newline-and-reformat, but force adaptive filling"
  (interactive)
  (let ((adaptive-fill-mode t))
    (message-newline-and-reformat)))

(put 'upcase-region 'disabled nil)


(defun update-diff-colors ()
  "update the colors for diff faces"
  ;(set-face-attribute 'diff-added nil
  ;                    :foreground "white" :background "blue")
  ;(set-face-attribute 'diff-removed nil
  ;                    :foreground "white" :background "red3")
  (set-face-attribute 'diff-added nil
                      :foreground "blue" :background "white")
  (set-face-attribute 'diff-removed nil
                      :foreground "red" :background "white")
  (set-face-attribute 'diff-changed nil
                      :foreground "green3" :background "white")
  (set-face-attribute 'diff-file-header nil
                      :foreground "purple" :background "white")
  (set-face-attribute 'diff-hunk-header nil
                      :foreground "yellow" :background "purple")
  )
(eval-after-load "diff-mode"
  '(update-diff-colors))

;(load "custom")
(require 'tramp)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ange-ftp-ftp-program-args nil)
 '(ange-ftp-ftp-program-name "sftp")
 '(ange-ftp-gateway-ftp-program-name "sftp")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(custom-safe-themes
   (quote
    ("8094177f57315be0278de74e1cbbc0cceaa823288cd479dc7b7de11e1b5b855a" "4a3f7f40fda830409fe0fe93c3379ade3ec33b1e6fd0a607c55d34ef6cc95763" "d6eedfc0013149f2f2ad8d44192e179af41c099131b4d24d7485ad5ee64ce392" "3683af725c9a22b9ff70427dbe410e23df12be43c4622bf29be6693748877923" "e556ba5ef0496a1b601854ad88e79877a7983e0091495df68a77821267528db8" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "dc16ca33b83579d105555fc31a990e4c1a4047627ec6b53da9eb78f8b9312ebf" "f82941a294a4268b66f9cf71f65934434a5f2c8db6645efc91bb62a176d5aca0" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "b946bb7354f8f468fb16218fa347034998b4812b758b8494d28ea686d977f1de" "eca7176eedb7b8a5b9e2a6500c7b2bc6b1e290dc5e405bc5f38e9a0b41122692" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "16e45b6dee0b6d1bf2d5dd8ccd1c5c69fbaa32432931ff84da6536f40eb8eac7" "20f8f3e47d55f73310e2376369e0e5ec7fd518b1a07821b97200c3833b0575d5" "c210c9e7116e4f899abd2f4409824a8ce0f9afcb284ba6c6b89a077eba1f57d6" "56ebbbe5158c1c4e2874aef0366874a4b9a28a705b52844ddd538c2a6dada9fb" "49a3c59e4b1ca3d1b2e4e19fbc41fa93e7f8613ff3d92010d90027125f1fe6da" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(electric-indent-mode nil)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#f6f0e1")
 '(frame-brackground-mode (quote dark))
 '(gnus-logo-colors (quote ("#0d7b72" "#adadad")))
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")))
 '(idlwave-help-use-assistant t)
 '(load-home-init-file t t)
 '(tramp-auto-save-directory "/home/shippert/tmp")
 '(vc-annotate-background "#f6f0e1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#e43838")
     (40 . "#f71010")
     (60 . "#ab9c3a")
     (80 . "#9ca30b")
     (100 . "#ef8300")
     (120 . "#958323")
     (140 . "#1c9e28")
     (160 . "#3cb368")
     (180 . "#028902")
     (200 . "#008b45")
     (220 . "#077707")
     (240 . "#259ea2")
     (260 . "#358d8d")
     (280 . "#0eaeae")
     (300 . "#2c53ca")
     (320 . "#1111ff")
     (340 . "#2020cc")
     (360 . "#a020f0"))))
 '(vc-annotate-very-old-color "#a020f0"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar ((t (:background "dim gray" :foreground "black")))))

(defun paragraph-to-line()
  "Convert paragraph to a single line, for insertion in web forms"
  (interactive)
  (save-excursion
    (replace-regexp "\\([^\n]\\)\n\\([^\n]\\)" "\\1 \\2")))

(defun unfill-paragraph () "Does the opposite of fill-paragraph"
       (interactive)
       (let ((fill-column (point-max))) (fill-paragraph nil)))

(defun date (arg)
  (interactive "P")
  (insert (if arg
	      (format-time-string "%Y%m%d")
	    (format-time-string "%-m/%-d/%Y"))))

(defun timestamp ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))
  
(setq minibuffer-max-depth nil)

(server-start)

(if (file-exists-p "~/.emacs.d/local-init.el")
    (load-file "~/.emacs.d/local-init.el"))
