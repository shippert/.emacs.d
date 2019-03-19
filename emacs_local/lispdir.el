;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lispdir.el --- Lisp Code Directory formatter and apropos
;; Authors         : Ashwin Ram (Ram-Ashwin@cs.yale.edu)
;;                 ; Dave Sill (de5@ornl.gov)
;;                 ; David Lawrence (tale@pawl.rpi.edu)
;;		   ; Noah Friedman (friedman@ai.mit.edu)
;;		   ; Joe Wells (jbw@maverick.uswest.com)
;;                 ; Dave Brennan (brennan@rtp.dg.com)
;; Created On      : Wed Jan 25, 1989
;; Last Modified By: Dave Brennan
;; Last Modified On: Fri Aug 23 00:05:47 1991
;; Update Count    : 24
;; Status          : No known bugs.
;; Version         : 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History
;; 22-Aug-1991		Dave Brennan
;;    Only load LCD-datafile when it is first referenced to make it faster.
;;    To force a reload kill the buffer " *LCD-datafile*" (note the space).

;; 03-Aug-1991		Dave Brennan
;;    If no matching entry is found print message in minibuffer instead of
;;    displaying an empty apropos buffer.  If the apropos buffer exists
;;    it is not modified.

;; 19-Jul-1991		Dave Sill	
;;    Added lisp-code-retrieve and lisp-code-verify using ange-ftp.

;; 18-Jul-1991		Dave Sill	
;;    Added "Archive" to Ram/Tale header. (Joe Wells <jbw@maverick.uswest.com>)

;; 12-Jul-1991		Noah Friedman (friedman@ai.mit.edu)
;;    Modified format-lisp-code-directory to use find-file-noselect in
;;    another buffer, then insert-buffer.  The reason for this is that
;;    insert-file has no hooks, and so cannot (for example) uncompress
;;    a compressed file.  This loses if I want to grab LCD-datafile.Z
;;    using ange-ftp.

;; 20-Jun-1991		de5
;;    Mostly cosmetic changes in prompts and buffer names.

;; 27-Jun-1989		dsill
;;    Added support for "archive" field containing anonymous FTP location.

;; 28-Feb-1989		dsill	
;;    Changed format-lcd-line-Sill to be smart about GNU-distributed code.
;;    Changed format-lcd-line-Sill to take advantage of 12-char max name.

;; 22-Feb-1989		dsill	
;;    Changed format-lisp-code-directory and lisp-dir-apropos to call the
;;      line formatter indirectly.  The variable 
;;      format-lisp-code-directory-line contains a function to format a single
;;      line, and format-lcd-line-Ram, format-lcd-line-tale, and
;;      format-lcd-line-Sill are the three possibilities at this time.

;; 20-Feb-1989		tale	
;;    changed file's name to lispdir.el
;;    format-lisp-code-directory makes separate buffer
;;    removed lisp-dir-apropos-buffer -- why use more space in memory?
;;    added lisp-dir-[apropos-]hook
;;      (I like (setq lisp-dir-hook 'delete-other-windows))
;;    other aesthetic changes

;; 16-Feb-1989		dsill	
;;    Added lisp-dir-apropos function

(require 'picture)			;provides move-to-column-force

(defvar lisp-code-directory
 "~/emacs/LCD-datafile"
  "*Database of free lisp code.  Entries are in the form:
Name|Author|Contact|Description|Date|Version|Archive")

(defvar format-lisp-code-directory-line 'format-lcd-line-Sill
  "*Function that formats one line of GNU Emacs Lisp Code Directory.\n
Provided as a variable for customizability.  Should not insert
final newline.")

(defvar lisp-code-directory-header 'lcd-header-Sill
  "*Function that inserts header appropriate for 
format-lisp-code-directory-line.")

(defvar elisp-archive-host "archive.cis.ohio-state.edu"
  "*Site with elisp archive available via anonymous ftp.")

(defvar elisp-archive-directory "/pub/gnu/emacs/elisp-archive/"
  "*Root directory of elisp archives on elisp-archive-host.")

(defun format-lisp-code-directory ()
   "Convert GNU Emacs Lisp Code Directory into something a human could read.
Calls value of lisp-dir-hook with no args if that value is non-nil."
   (interactive)
   (pop-to-buffer "*GNU Emacs Lisp Code Directory*")
   (fundamental-mode)
   (setq buffer-read-only nil)
   (erase-buffer)
   (buffer-flush-undo (current-buffer))
   (lisp-dir-insert-datafile)
   (insert " GNU Emacs Lisp code directory.  " (current-time-string) ".\n\n")
   (message "Formatting %s ..." lisp-code-directory)
   (delete-region (progn (beginning-of-line) (point))
		  (progn (end-of-line) (point)))
   (funcall lisp-code-directory-header)
   (while (re-search-forward
	   "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
      (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
            (author (buffer-substring (match-beginning 2) (match-end 2)))
            (contact (buffer-substring (match-beginning 3) (match-end 3)))
            (description (buffer-substring (match-beginning 4) (match-end 4)))
            (date (buffer-substring (match-beginning 5) (match-end 5)))
            (version (buffer-substring (match-beginning 6) (match-end 6)))
	    (archive (buffer-substring (match-beginning 7) (match-end 7))))
       (delete-region (progn (beginning-of-line) (point))
	(progn (end-of-line) (point)))
       (funcall format-lisp-code-directory-line
	name author contact description date version archive)))
   (goto-char (point-min))
   (center-line)
   (message "Formatting %s ... done" lisp-code-directory)
   (set-buffer-modified-p nil)
   (run-hooks 'lisp-dir-hook))

(defun lisp-dir-apropos (topic)
  "Display entries in Lisp Code Directory for TOPIC in separate window.
Calls value of lisp-dir-apropos-hook with no args if that value is non-nil."
  (interactive (list
		(read-string
		 (concat "GELCD apropos (" (current-word) "): "))))
  (if (equal "" topic) (setq topic (current-word)))
  (save-excursion
    (let ((lisp-code-directory-tmp-buffer
	   (get-buffer-create "*lcd-working*")))
      (message "Searching for %s ..." topic)
      (set-buffer lisp-code-directory-tmp-buffer)
      (lisp-dir-insert-datafile)
      (delete-non-matching-lines topic)
      (set-buffer-modified-p nil)
      (if (= (point-min) (point-max))
	  (progn
	    (kill-buffer lisp-code-directory-tmp-buffer)
	    (message "No entries matching `%s' were found." topic))
	(set-buffer
	 (get-buffer-create "*GNU Emacs Lisp Code Directory Apropos*"))
	(fundamental-mode)
	(setq buffer-read-only nil)
	(erase-buffer)
	(buffer-flush-undo (current-buffer))
	(insert-buffer lisp-code-directory-tmp-buffer)
	(kill-buffer lisp-code-directory-tmp-buffer)
	(insert "GNU Emacs Lisp Code Directory Apropos -- \"" topic "\"\n\n\n")
	(backward-char 1)
	(funcall lisp-code-directory-header)
	(while (re-search-forward
	"\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
	  (let ((name (buffer-substring (match-beginning 1) (match-end 1)))
		(author (buffer-substring (match-beginning 2) (match-end 2)))
		(contact (buffer-substring (match-beginning 3) (match-end 3)))
		(description (buffer-substring (match-beginning 4) (match-end 4)))
		(date (buffer-substring (match-beginning 5) (match-end 5)))
		(version (buffer-substring (match-beginning 6) (match-end 6)))
		(archive (buffer-substring (match-beginning 7) (match-end 7))))
	    (delete-region (progn (beginning-of-line) (point))
			   (progn (end-of-line) (point)))
	    (funcall format-lisp-code-directory-line
		     name author contact description date version archive)))
	(goto-char (point-min))
	(center-line)
	(message "Searching for %s ... done" topic)
	(set-buffer-modified-p nil)
	(display-buffer "*GNU Emacs Lisp Code Directory Apropos*")
	(run-hooks 'lisp-dir-apropos-hook)))))

;; Read in lisp code directory file in another buffer (using
;; find-file-noselect, so that usual find-file-hooks will be run,
;; like find-crypt-file-hook). 

(defun lisp-dir-insert-datafile ()
  "Insert the LCD-database in the current buffer.  If the file isn't already
in memory it is read into the buffer \" *LCD-datafile*\".  The leading space
prevents the buffer from showing up in buffer lists."
  (if (not (get-buffer " *LCD-datafile*"))
      (save-excursion
	(set-buffer (find-file-noselect lisp-code-directory))
	(rename-buffer " *LCD-datafile*")
	(setq buffer-read-only t)))
  (insert-buffer " *LCD-datafile*")
  (setq buffer-read-only nil))

(defun format-lcd-line-Ram
  (name author contact description date version archive)
  "Columnar formatter for Lisp code directory that tries to use as few lines
as possible.  Doesn't fit Contact within first 80 columns."
   (insert-at-column 1  name)
   (insert-at-column 17 description)
   (insert-at-column 49 author)
   (insert-at-column 65 date)
   (insert-at-column 74 "/")
   (insert-at-column 75 version)
   (insert-at-column 84 contact))

(defun format-lcd-line-tale
  (name author contact description date version archive)
  "Multi-line columnar formatter for Lisp Code Directory that tries not
to write anything past column 79."
   (insert-at-column 0  name)
   (insert-at-column 17 description)
   (insert-at-column 56 author)
   (insert-at-column 4  contact)
   (insert-at-column 56 date)
   (insert-at-column 72 version))

(defun format-lcd-line-Sill
  (name author contact description date version archive)
  "Multi-line non-columnar line formatter for Lisp Code Directory."
  (insert-at-column 0 name)
  (if (not (equal version ""))
      (insert " (" version ")"))
  (insert-at-column 18 date)
  (insert "\n")
  (if (and (string-match "[0-9]+\.[0-9]+ dist" contact)
	   (equal author "FSF"))
      (insert-at-column 5 contact)
    (progn
      (insert-at-column 5 author)
      (insert ", <" contact ">\n")
      (if (not (equal archive ""))
	  (progn
	    (if (and (string-match "~" archive)
                     (= 0 (string-match "~" archive)))
		(setq archive (concat elisp-archive-host ":" elisp-archive-directory
				      (substring archive 2))))
	    (insert-at-column 5 archive)))))
  (insert-at-column 5 description))

(defun lcd-header-Ram/tale ()
  "Inserts header for column-formatted Lisp Code Directory."
  (funcall format-lisp-code-directory-line
    "Name" "Author" "Contact" "Description" "Date" "Version" "Archive")
  (insert "\n")
  (insert-char ?- 79)
)

(defun lcd-header-Sill ()
  "Inserts empty header for non-columnar Lisp Code Directory"
)

(defun insert-at-column (col string)
   (if (> (current-column) col) (insert "\n"))
   (move-to-column-force col)
   (insert string))

(defun lisp-dir-retrieve (name)
  "Retrieves a copy of the NAMEd package using ange-ftp.
Calls value of lisp-dir-retrieve-hook with no args if that value is non-nil."
  (interactive (list
		(read-string
		 (concat "GELCD retrieve (" (current-word) "): "))))
  (if (equal "" name) (setq name (current-word)))
  (save-excursion
    (require 'ange-ftp)
    (set-buffer (get-buffer-create (concat "GELCD-" name)))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (lisp-dir-insert-datafile)
    (message "Searching for %s ..." name)
    (delete-non-matching-lines (concat "^" name "|"))
    (let ((matches (count-lines (point-min) (point-max))))
      (cond ((= matches 0)
             (progn
               (message "No match found for %s" name)
               nil))
            ((> matches 1)
             (progn
               (message "Multiple matches found for %s, should be unique" name)
               nil))
            (t
             (re-search-forward
              "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
             (let ((archive (buffer-substring (match-beginning 7) (match-end 7))))
               (if (not (equal archive ""))
                   (progn
                     (if (and (string-match "~" archive)
                              (= 0 (string-match "~" archive)))
                         (setq archive (concat elisp-archive-host ":"
                                               elisp-archive-directory
                                               (substring archive 2))))))
               (erase-buffer)
               (let ((lisp-code-directory-tmp-buffer
                      (find-file-noselect (concat "/anonymous@" archive))))
                 (insert-buffer lisp-code-directory-tmp-buffer)
                 (kill-buffer lisp-code-directory-tmp-buffer)))
             (goto-char (point-min))
             (display-buffer (concat "GELCD-" name))
             (run-hooks 'lisp-dir-retrieve-hook))))))

(defun lisp-dir-verify (name)
  "Verifies the archive location of the NAMEd package using ange-ftp."
  (interactive (list
		(read-string
		 (concat "GELCD verify (" (current-word) "): "))))
  (if (equal "" name) (setq name (current-word)))
  (save-excursion
    (require 'ange-ftp)
    (set-buffer (get-buffer-create "GELCD-verify"))
    (fundamental-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (lisp-dir-insert-datafile)
    (message "Searching for %s ..." name)
    (delete-non-matching-lines (concat "^" name "|"))
    (let ((matches (count-lines (point-min) (point-max))))
      (cond ((= matches 0)
             (progn
               (message "No match found for %s" name)
               nil))
            ((> matches 1)
             (progn
               (message "Multiple matches found for %s, should be unique" name)
               nil))
            (t
             (re-search-forward
              "\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)|\\(.*\\)" nil t)
             (let ((archive (buffer-substring (match-beginning 7) (match-end 7))))
               (if (not (equal archive ""))
                   (progn
                     (if (and (string-match "~" archive)
                              (= 0 (string-match "~" archive)))
                         (setq archive (concat elisp-archive-host ":"
                                               elisp-archive-directory
                                               (substring archive 2))))
                     (if (ange-ftp-file-exists-p (concat "/anonymous@" archive))
                         (message "Package %s is available from: %s" name archive)
                       (message "Package %s is supposed to be available but isn't." name)))
                 (message "Package %s is not archived." name))))))))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg end)
	 (re-search-backward "\\w" nil 2)
	 (re-search-backward "\\b" nil 2)
	 (setq beg (point))
	 (re-search-forward "\\w*\\b" nil 2)
	 (setq end (point))
	 (buffer-substring beg end))))

(provide 'lispdir)
