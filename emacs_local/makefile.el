
;; Makefile editing commands for Emacs
;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is *NOT* part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; ------------------------------------------------------------
;;; $Id: makefile.el,v 1.7.1.4 1992/07/06 15:02:09 tom Exp tom $
;;;
;;; written 1992 by Thomas Neumann <tom@smart.bo.open.de>
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; Configureable stuff
;;; ------------------------------------------------------------

(defvar makefile-mode-name "makefile"
  "The \"pretty name\" of makefile-mode, as it
appears in the modeline.")

(defvar makefile-browser-buffer-name "*Macros and Targets*"
  "Name of the macro- and target browser buffer.")

(defvar makefile-target-colon ":"
  "The string that gets appended to all target names
inserted by makefile-insert-target.
\":\" or \"::\" are quite common values.")

(defvar makefile-macro-assign " = "
  "The string that gets appended to all macro names
inserted by makefile-insert-macro.
The normal value should be \" = \", since this is what
standard make expects. However, newer makes such as dmake
allow a larger variety of different macro assignments, so you
might prefer to use \" += \" or \" := \" .")

(defvar makefile-tab-after-target-colon t
  "If you want a TAB (instead of a space) to be appended after the
target colon, then set this to a non-nil value.")

(defvar makefile-browser-leftmost-column 10
  "Number of blanks to the left of the browser selection mark.")

(defvar makefile-browser-cursor-column 10
  "Column in which the cursor is positioned when it moves
up or down in the browser.")

(defvar makefile-browser-selected-mark "+  "
  "String used to mark selected entries in the browser.")

(defvar makefile-browser-unselected-mark "   "
  "String used to mark unselected entries in the browser.")

(defvar makefile-browser-auto-advance-after-selection-p t
  "If this variable is set to a non-nil value the cursor
will automagically advance to the next line after an item
has been selected in the browser.")

(defvar makefile-find-file-autopickup-p t
  "If this variable is set to a non-nil value then finding a file in
a makefile-mode buffer will cause an automatic initial pickup of
all macros and targets from the found file.")

(defvar makefile-pickup-everything-picks-up-filenames-p nil
  "If this variable is set to a non-nil value then
makefile-pickup-everything also picks up filenames as targets
(i.e. it calls makefile-find-filenames-as-targets), otherwise
filenames are omitted.")

(defvar makefile-browser-hook '()
  "A function or list of functions to be called just before the
browser is entered. This is executed in the makefile buffer, so
you can for example run a makefile-pickup-everything automatically.")

;;
;; The list below is somewhat tuned for dmake ...
;; 
(defvar makefile-special-targets-list
  '(("ERROR")
    ("EXPORT")
    ("GROUPEPILOG")
    ("GROUPPROLOG")
    ("IGNORE")
    ("IMPORT")
    ("INCLUDE")
    ("INCLUDEDIRS")
    ("MAKEFILES")
    ("PHONY")
    ("PRECIOUS")
    ("REMOVE")
    ("SOURCE")
    ("SUFFIXES")
    ("c.o")
    ("C.o")
    ("m.o")
    ("el.elc")
    ("y.c")
    ("s.o"))
  "List of special targets. You will be offered to complete
on one of those in the minibuffer whenever you enter a \".\"
at the beginning of a line in makefile-mode.")

(defconst makefile-dependency-regex
  "^\\([^=:# \t]+[ \t]*\\)+:.*$"
  "Regex used to find dependency lines in a makefile.")

(defconst makefile-macroassign-regex
  "^[^ \t][^:#=]*[\\*:\\+]?:?=.*$"
  "Regex used to find macro assignment lines in a makefile.")

(defconst makefile-ignored-files-in-pickup-regex
  "\\(^\\..*\\)\\|\\(.*~$\\)\\|\\(.*,v$\\)"
  "Regex for filenames that will NOT be included in the target list.")

(defvar makefile-mode-map nil
  "The keymap that is used in makefile-mode.")
(if makefile-mode-map
    ()
  (setq makefile-mode-map (make-sparse-keymap))
  ;; set up the keymap
  (define-key makefile-mode-map "$"        'makefile-insert-macro-ref)
  (define-key makefile-mode-map "\C-c:"    'makefile-insert-target-ref)
  (define-key makefile-mode-map ":"        'makefile-electric-colon)
  (define-key makefile-mode-map "="        'makefile-electric-equal)
  (define-key makefile-mode-map "."        'makefile-electric-dot)
  (define-key makefile-mode-map "\C-c\C-t" 'makefile-pickup-targets)
  (define-key makefile-mode-map "\C-c\C-m" 'makefile-pickup-macros)
  (define-key makefile-mode-map "\C-c\C-f" 'makefile-pickup-filenames-as-targets)
  (define-key makefile-mode-map "\C-c\C-0" 'makefile-forget-everything)
  (define-key makefile-mode-map "\C-c0"    'makefile-forget-everything)  
  (define-key makefile-mode-map "\C-c\C-b" 'makefile-switch-to-browser)
  (define-key makefile-mode-map "\C-c\C-p" 'makefile-pickup-everything)
  (define-key makefile-mode-map "\M-p"     'makefile-previous-dependency)
  (define-key makefile-mode-map "\M-n"     'makefile-next-dependency))  

(defvar makefile-browser-map nil
  "The keymap that is used in the macro- and target browser.")
(if makefile-browser-map
    ()
  (setq makefile-browser-map (make-sparse-keymap))
  (define-key makefile-browser-map "n"    'makefile-browser-next-line)
  (define-key makefile-browser-map "\C-n" 'makefile-browser-next-line)    
  (define-key makefile-browser-map "p"    'makefile-browser-previous-line)
  (define-key makefile-browser-map "\C-p" 'makefile-browser-previous-line)
  (define-key makefile-browser-map " "    'makefile-browser-toggle)
  (define-key makefile-browser-map "i"    'makefile-browser-insert-selection)
  (define-key makefile-browser-map "I"    'makefile-browser-insert-selection-and-quit)  
  (define-key makefile-browser-map "\C-c\C-m" 'makefile-browser-insert-continuation)
  (define-key makefile-browser-map "q"    'makefile-browser-quit)
  ;; disable horizontal movement
  (define-key makefile-browser-map "\C-b" 'undefined)
  (define-key makefile-browser-map "\C-f" 'undefined))  


;;; ------------------------------------------------------------
;;; Internal variables.
;;; You don't need to configure below this line.
;;; ------------------------------------------------------------

(defvar makefile-target-table nil
  "Table of all targets that have been inserted in
this Makefile buffer using makefile-insert-target or picked up
using makefile-pickup-targets.")

(defvar makefile-macro-table nil
  "Table of all macros that have been iserted in
this Makefile buffer using makefile-insert-macro or picked up
using makefile-pickup-macros.")

(defvar makefile-browser-client
  "A buffer in makefile-mode that is currently using the browser.")

(defvar makefile-browser-selection-vector nil)

(defvar makefile-mode-hook '())

;;; ------------------------------------------------------------
;;; The mode function itself.
;;; ------------------------------------------------------------

(defun makefile-mode ()
  "Major mode for editing Makefiles.
Calling this function invokes the function(s) \"makefile-mode-hook\" before
doing anything else.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

makefile-mode can be configured by modifying the following
variables:

makefile-mode-name:
    The \"pretty name\" of makefile-mode, as it
    appears in the modeline.

makefile-browser-buffer-name:
    Name of the macro- and target browser buffer.

makefile-target-colon:
    The string that gets appended to all target names
    inserted by makefile-insert-target.
    \":\" or \"::\" are quite common values.

makefile-macro-assign:
   The string that gets appended to all macro names
   inserted by makefile-insert-macro.
   The normal value should be \" = \", since this is what
   standard make expects. However, newer makes such as dmake
   allow a larger variety of different macro assignments, so you
   might prefer to use \" += \" or \" := \" .

makefile-tab-after-target-colon:
   If you want a TAB (instead of a space) to be appended after the
   target colon, then set this to a non-nil value.

makefile-browser-leftmost-column:
   Number of blanks to the left of the browser selection mark.

makefile-browser-cursor-column:
   Column in which the cursor is positioned when it moves
   up or down in the browser.

makefile-browser-selected-mark:
   String used to mark selected entries in the browser.

makefile-browser-unselected-mark:
   String used to mark unselected entries in the browser.

makefile-browser-auto-advance-after-selection-p:
   If this variable is set to a non-nil value the cursor
   will automagically advance to the next line after an item
   has been selected in the browser.

makefile-find-file-autopickup-p:
   If this variable is set to a non-nil value then finding a file in
   a makefile-mode buffer will cause an automatic initial pickup of
   all macros and targets from the found file.

makefile-pickup-everything-picks-up-filenames-p:
   If this variable is set to a non-nil value then
   makefile-pickup-everything also picks up filenames as targets
   (i.e. it calls makefile-find-filenames-as-targets), otherwise
   filenames are omitted.

makefile-browser-hook:
   A function or list of functions to be called just before the
   browser is entered. This is executed in the makefile buffer, so
   you can for example run a makefile-pickup-everything automatically.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a \".\"
   at the beginning of a line in makefile-mode."
  (interactive)
  (kill-all-local-variables)
  (run-hooks 'makefile-mode-hook)
  (if (not (memq 'makefile-find-file-autopickup find-file-hooks))
      (setq find-file-hooks
	    (append find-file-hooks (list 'makefile-find-file-autopickup))))
  (make-variable-buffer-local 'makefile-target-table)
  (make-variable-buffer-local 'makefile-macro-table)
  (makefile-forget-all-macros)
  (makefile-forget-all-targets)
  (setq comment-start "#")
  (setq comment-end "")
  ;; become the current major mode
  (setq major-mode 'makefile-mode)
  (setq mode-name makefile-mode-name)
  ;; activate keymap
  (use-local-map makefile-mode-map))


(defun makefile-find-file-autopickup ()
  (if (eq major-mode 'makefile-mode)
      (if makefile-find-file-autopickup-p
	  (makefile-pickup-everything))))

(defun makefile-next-dependency ()
  "Move (point) to the beginning of the next dependency line
below the current position of (point)."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (re-search-forward makefile-dependency-regex (point-max) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))
      
(defun makefile-previous-dependency ()
  "Move (point) to the beginning of the next dependency line
above the current position of (point)."
  (interactive)
  (let ((here (point)))
    (beginning-of-line)
    (if (re-search-backward makefile-dependency-regex (point-min) t)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))


(defun makefile-electric-dot ()
  "At (bol), offer completion on makefile-special-targets-list.
Anywhere else just insert a dot."
  (interactive)
  (if (bolp)
      (makefile-insert-special-target)
    (insert ".")))


(defun makefile-insert-special-target ()
  "Offer completion on makefile-special-targets-list and insert
the result at (point)."
  (interactive)
  (let
      ((special-target
       (completing-read "Special target: "
			makefile-special-targets-list nil nil nil)))
    (if (zerop (length special-target))
	()
      (insert (format ".%s:" special-target))
      (makefile-forward-after-target-colon))))


(defun makefile-electric-equal ()
  "At (bol) do makefile-insert-macro. Anywhere else just
self-insert."
  (interactive)
  (if (bolp)
      (call-interactively 'makefile-insert-macro)
    (insert "=")))

(defun makefile-insert-macro (macro-name)
  "Prepare definition of a new macro."
  (interactive "sMacro Name: ")
  (if (not (zerop (length macro-name)))
      (progn
	(beginning-of-line)
	(insert (format "%s%s" macro-name makefile-macro-assign))
	(makefile-remember-macro macro-name))))


(defun makefile-insert-macro-ref (macro-name)
  "Offer completion on a list of known macros, then
insert complete macro-ref at (point) ."
  (interactive
   (list
    (completing-read "Refer to macro: " makefile-macro-table nil nil nil)))
   (if (not (zerop (length macro-name)))
       (progn
	 (insert (format "${%s} " macro-name)))))


(defun makefile-insert-target (target-name)
  "Prepare definition of a new target (dependency line)."
  (interactive "sTarget: ")
  (if (not (zerop (length target-name)))
      (progn
	(beginning-of-line)
	(insert (format "%s%s" target-name makefile-target-colon))
	(makefile-forward-after-target-colon)
	(end-of-line)
	(makefile-remember-target target-name))))


(defun makefile-insert-target-ref (target-name)
  "Offer completion on a list of known targets, then
insert complete target-ref at (point) ."
  (interactive
   (list
    (completing-read "Refer to target: " makefile-target-table nil nil nil)))
   (if (not (zerop (length target-name)))
       (progn
	 (insert (format "%s " target-name)))))

(defun makefile-electric-colon ()
  "At (bol) defines a new target, anywhere else just self-insert ."
  (interactive)
  (if (bolp)
      (call-interactively 'makefile-insert-target)
    (insert ":")))


;;; ------------------------------------------------------------
;;; Extracting targets and macros from an existing makefile
;;; ------------------------------------------------------------

(defun makefile-pickup-targets ()
  "Scan a buffer that contains a makefile for target definitions (dependencies)
and add them to the list of known targets."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward makefile-dependency-regex (point-max) t)
      (makefile-add-this-line-targets)
      (forward-line 1))))

(defun makefile-add-this-line-targets ()
  (beginning-of-line)
  (if (re-search-forward makefile-macroassign-regex (point-max) t)
      ()
    (beginning-of-line)
    (let ((done-with-line nil))
      (while (not done-with-line)
	(skip-chars-forward " \t")
	(if (not (setq done-with-line (or (eolp)
					  (char-equal (char-after (point)) ?:))))
	    (progn
	      (let* ((start-of-target-name (point))
		     (target-name (progn
				    (skip-chars-forward "^ \t:#")
				    (buffer-substring start-of-target-name (point)))))
		(if (makefile-remember-target target-name)
		    (message "Picked up target \"%s\"" target-name)))))))))


(defun makefile-pickup-macros ()
  "Scan a buffer that contains a makefile for macro definitions
and add them to the list of known macros."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward makefile-macroassign-regex (point-max) t)
      (makefile-add-this-line-macro)
      (forward-line 1))))

(defun makefile-add-this-line-macro ()
  (beginning-of-line)
  (skip-chars-forward " \t")
  (if (not (eolp))
      (let* ((start-of-macro-name (point))
	     (macro-name (progn
			   (skip-chars-forward "^ \t:#=*")
			   (buffer-substring start-of-macro-name (point)))))
	(if (makefile-remember-macro macro-name)
	    (message "Picked up macro \"%s\"" macro-name)))))


(defun makefile-pickup-everything ()
  "Calls makefile-pickup-targets and makefile-pickup-macros.
See their documentation for what they do."
  (interactive)
  (makefile-pickup-targets)
  (makefile-pickup-macros)
  (if makefile-pickup-everything-picks-up-filenames-p
      (makefile-pickup-filenames-as-targets)))


(defun makefile-pickup-filenames-as-targets ()
  "Scan the current directory for filenames, check each filename
against makefile-ignored-files-in-pickup-regex and add all qualifying
names to the list of known targets."
  (interactive)
  (let* ((dir (file-name-directory (buffer-file-name)))
	 (raw-filename-list (if dir
				(file-name-all-completions "" dir)
			      (file-name-all-completions "" ""))))
    (mapcar '(lambda (name)
	       (if (and (not (file-directory-p name))
			(not (string-match makefile-ignored-files-in-pickup-regex
					   name)))
		   (if (makefile-remember-target name)
		       (message "Picked up file \"%s\" as target" name))))
	    raw-filename-list)))

;;; ------------------------------------------------------------
;;; The browser window
;;; ------------------------------------------------------------


(defun makefile-browser-format-target-line (target selected)
  (format
   (concat (make-string makefile-browser-leftmost-column ?\ )
	   (if selected
	       makefile-browser-selected-mark
	     makefile-browser-unselected-mark)
	   "%s%s")
   target makefile-target-colon))

(defun makefile-browser-format-macro-line (macro selected)
  (format
   (concat (make-string makefile-browser-leftmost-column ?\ )
	   (if selected
	       makefile-browser-selected-mark
	     makefile-browser-unselected-mark)
	   "${%s}")
   macro))


(defun makefile-browser-fill (targets macros)
  (setq buffer-read-only nil)
  (goto-char (point-min))
  (erase-buffer)
  (mapconcat
   (function
    (lambda (item) (insert (makefile-browser-format-target-line (car item) nil) "\n")))
   targets
   "")
  (mapconcat
   (function
    (lambda (item) (insert (makefile-browser-format-macro-line (car item) nil) "\n")))
   macros
   "")
  (sort-lines nil (point-min) (point-max))
  (goto-char (1- (point-max)))
  (delete-char 1)			; remove unnecessary newline at eob
  (goto-char (point-min))
  (forward-char makefile-browser-cursor-column)
  (setq buffer-read-only t))
  

;;;
;;; Moving up and down in the browser
;;;

(defun makefile-browser-next-line ()
  "Move the browser selection cursor to the next line."
  (interactive)
  (if (not (makefile-last-line-p))
      (progn
	(forward-line 1)
	(forward-char makefile-browser-cursor-column))))

(defun makefile-browser-previous-line ()
  "Move the browser selection cursor to the previous line."
  (interactive)
  (if (not (makefile-first-line-p))
      (progn
	(forward-line -1)
	(forward-char makefile-browser-cursor-column))))

;;;
;;; Quitting the browser (returns to client buffer)
;;;

(defun makefile-browser-quit ()
  "Leave the makefile-browser-buffer and return to the buffer
from that it has been entered."
  (interactive)
  (let ((my-client makefile-browser-client))
    (setq makefile-browser-client nil)	; we quitted, so NO client!
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (pop-to-buffer my-client)))

;;;
;;; Toggle state of a browser item
;;;

(defun makefile-browser-toggle ()
  "Toggle the selection state of the browser item at the cursor position."
  (interactive)
  (let ((this-line (count-lines (point-min) (point))))
    (setq this-line (max 1 this-line))
    (makefile-browser-toggle-state-for-line this-line)
    (goto-line this-line)
    (setq buffer-read-only nil)
    (beginning-of-line)
    (if (makefile-browser-on-macro-line-p)
	(let ((macro-name (makefile-browser-this-line-macro-name)))
	  (kill-line)
	  (insert
	   (makefile-browser-format-macro-line
	      macro-name
	      (makefile-browser-get-state-for-line this-line))))
      (let ((target-name (makefile-browser-this-line-target-name)))
	(kill-line)
	(insert
	 (makefile-browser-format-target-line
	    target-name
	    (makefile-browser-get-state-for-line this-line)))))
    (setq buffer-read-only t)
    (beginning-of-line)
    (forward-char makefile-browser-cursor-column)
    (if makefile-browser-auto-advance-after-selection-p
	(makefile-browser-next-line))))

;;;
;;; Making insertions into the client buffer
;;;

(defun makefile-browser-insert-continuation ()
  "In the browser\'s client buffer, go to (end-of-line), insert a \'\\\'
character, insert a new blank line, go to that line and indent by one TAB.
This is most useful in the process of creating continued lines when 'sending' large
dependencies from the browser to the client buffer.
(point) advances accordingly in the client buffer."
  (interactive)
  (save-excursion
    (set-buffer makefile-browser-client)
    (end-of-line)
    (insert "\\\n\t")))

(defun makefile-browser-insert-selection ()
  "Insert all browser-selected targets and/or macros in the browser\'s
client buffer.
Insertion takes place at (point)."
  (interactive)
  (save-excursion
    (goto-line 1)
    (let ((current-line 1))
      (while (not (eobp))
	(if (makefile-browser-get-state-for-line current-line)
	    (makefile-browser-send-this-line-item))
	(forward-line 1)
	(setq current-line (1+ current-line))))))

(defun makefile-browser-insert-selection-and-quit ()
  (interactive)
  (makefile-browser-insert-selection)
  (makefile-browser-quit))

(defun makefile-browser-send-this-line-item ()
  (if (makefile-browser-on-macro-line-p)
      (save-excursion
	(let ((macro-name (makefile-browser-this-line-macro-name)))
	  (set-buffer makefile-browser-client)
	  (insert (format "${%s} " macro-name))))
    (save-excursion
      (let ((target-name (makefile-browser-this-line-target-name)))
	(set-buffer makefile-browser-client)
	(insert target-name " ")))))


(defun makefile-browser-start-interaction ()
  (use-local-map makefile-browser-map)
  ;; disable horizontal movement
  (local-set-key "\C-b" (function (lambda () (interactive) (beep))))
  (local-set-key "\C-f" (function (lambda () (interactive) (beep))))
  (setq buffer-read-only t))


(defun makefile-browse (targets macros)
  (interactive)
  (if (zerop (+ (length targets) (length macros)))
      (progn
	(beep)
	(message "No macros or targets to browse! Consider running 'makefile-pickup-everything\'"))
    (let ((browser-buffer (get-buffer-create makefile-browser-buffer-name)))
	(pop-to-buffer browser-buffer)
	(make-variable-buffer-local 'makefile-browser-selection-vector)
	(makefile-browser-fill targets macros)
	(setq makefile-browser-selection-vector
	      (make-vector (+ (length targets) (length macros)) nil))
	(makefile-browser-start-interaction))))
  

(defun makefile-switch-to-browser ()
  (interactive)
  (run-hooks 'makefile-browser-hook)
  (setq makefile-browser-client (current-buffer))
  (makefile-browse makefile-target-table makefile-macro-table))


;;; ------------------------------------------------------------
;;; Utility functions
;;; ------------------------------------------------------------

(defun makefile-forget-all-targets ()
  "Clear the target-table for this buffer."
  (interactive)
  (setq makefile-target-table '()))

(defun makefile-forget-all-macros ()
  "Clear the macro-table for this buffer."
  (interactive)
  (setq makefile-macro-table '()))


(defun makefile-forget-everything ()
  "Clear the macro-table AND the target-table for this buffer."
  (interactive)
  (if (y-or-n-p "Really forget all macro- and target information ? ")
      (progn
	(makefile-forget-all-targets)
	(makefile-forget-all-macros)
	(if (get-buffer makefile-browser-buffer-name)
	    (kill-buffer makefile-browser-buffer-name))
	(message "Cleared macro- and target tables."))))

(defun makefile-remember-target (target-name)
  "Remember a given target if it is not already remembered for this buffer."
  (if (not (assoc target-name makefile-target-table))
      (setq makefile-target-table
	    (cons (list target-name) makefile-target-table))))

(defun makefile-remember-macro (macro-name)
  "Remember a given macro if it is not already remembered for this buffer."
  (if (not (assoc macro-name makefile-macro-table))
      (setq makefile-macro-table
	    (cons (list macro-name) makefile-macro-table))))

(defun makefile-forward-after-target-colon ()
"Move point forward after the terminating colon
of a target has been inserted.
This accts according to the value of makefile-tab-after-target-colon ."
  (if makefile-tab-after-target-colon
      (insert "\t")
    (insert " ")))

(defun makefile-browser-on-macro-line-p ()
  (save-excursion
    (beginning-of-line)
    (search-forward "${" (makefile-end-of-line-point) t)))

(defun makefile-browser-this-line-target-name ()
  (save-excursion
    (end-of-line)
    (skip-chars-backward "^ \t")
    (buffer-substring (point) (1- (makefile-end-of-line-point)))))

(defun makefile-browser-this-line-macro-name ()
  (save-excursion
    (beginning-of-line)
    (search-forward "${" (makefile-end-of-line-point) t)
    (let ((macro-start (point)))
      (skip-chars-forward "^}")
      (buffer-substring macro-start (point)))))

(defun makefile-browser-get-state-for-line (n)
  (aref makefile-browser-selection-vector (1- n)))

(defun makefile-browser-set-state-for-line (n to-state)
  (aset makefile-browser-selection-vector (1- n) to-state))

(defun makefile-browser-toggle-state-for-line (n)
  (makefile-browser-set-state-for-line n (not (makefile-browser-get-state-for-line n))))

(defun makefile-beginning-of-line-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun makefile-end-of-line-point ()
  (save-excursion
    (end-of-line)
    (point)))

(defun makefile-last-line-p ()
  (= (makefile-end-of-line-point) (point-max)))

(defun makefile-first-line-p ()
  (= (makefile-beginning-of-line-point) (point-min)))


; === end of makefile.el ======================================

