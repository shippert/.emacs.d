;; Emacs-lisp to font-lock and then print (and exit from) a buffer.
;; Called from command line like this:
;;   emacs -Q --file=$nu -l ~/.emacs.d/eprint.el
;; See ~/bin/emacs_print for my shell script that does this inside
;; xvfb, so no window shows up.

;; Derived from my (reduced) init file, to get the font-locking
;; similar, with hopefully all the extraneous garbage removed.

(turn-on-auto-fill)

;(require 'diff-mode-.el)

; for TM
;(setenv "TM_TMP_DIR" "/home/d3a230/mime")

;; I'm not supposed to do this, but I don't like dark blue
;; Has to be in .emacs, before we run gnus down there with the TM stuff
(setq gnus-face-dark-name-list
  (list "blue3" "firebrick" "dark green" "OrangeRed" "dark khaki"
	 "dark violet" "SteelBlue4")) 

;; set my faces
;; New face here for green - so I can keep underlining.
(make-face 'emphasize)

(setq load-path
 (append (list (expand-file-name "~/emacs")
	       (expand-file-name "/usr/local/lib/emacs/site-lisp")
	       )
	 load-path))

(require 'generic )

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

;; NOTES mode autoload - load notes.el
(autoload 'notes-mode	   "notes" "Notes mode" t)
(autoload 'dod-mode	   "dod" "DOD mode" t)

; Misc setup
;;(setq mail-archive-blind-file "~/mail/archive")
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
(require 'pod-mode )

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
(load "/home/shippert/emacs/ess-5.3.7/lisp/ess-site.el" t)

;;;
;;;(load "sendmail")
(load "tim" t)

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
 '(display-time-mode t)
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
  
(setq minibuffer-max-depth nil)

(font-lock-fontify-buffer)

(load-theme 'shippert 't)

; Fix some faces that don't work right while printing. This should
; modify the faces in place.  I discovered this when I thought I
; couldn't print the background - this lets you modify the faces if
; you need different ones for printing than for in the buffer.
;(ps-extend-face '(list-face "blue" "yellow" bold))
;(ps-extend-face '(red-face "red" "yellow" box) 'MERGE)
;(ps-extend-face '(red-face "red" "yellow" bold) 'MERGE)
;(ps-extend-face '(zoinks-face "green" "black" bold) 'MERGE)
;(ps-extend-face '(zoinks-face "green" "black" box) 'MERGE)
;(ps-extend-face '(loud-face "orange" "black" bold))

; Turn on background for the emphasized fonts.  So simple, but so hard
; to find.
(setq ps-use-face-background 't)

(setq font-lock-maximum-decoration t)

(global-font-lock-mode t)
; ;;; set my faces
(font-lock-mode 1)

;; Print the line number
(setq ps-line-number 't)

;; Reload the buffer to apply this font-locking
(revert-buffer 't 't)

;; Now print and exit
(ps-print-buffer-with-faces (concat (file-name-nondirectory
				     buffer-file-name)
				    ".ps"))
(save-buffers-kill-terminal t)

