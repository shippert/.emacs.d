;; Print Emacs buffer on line printer.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'mprint)

(defvar mprint-switches nil
  "*List of strings to pass as extra switch args to `mprint-command' when it
 is invoked.") 

(defvar mprint-command "lpr"
  "Command to print things out")

(defvar mprint-printer (getenv "PRINTER")
  "Default printer to print things to.")

(defun mprint-buffer ()
  "Print buffer contents with command given by `mprint-command'.
 `mprint-switches' is a list of extra switches (strings) to pass to
 `mprint-command'."   
  (interactive)
  (mprint-region-1 (point-min) (point-max) (list mprint-switches
						 (concat " -P "
							 mprint-printer)
						 )))

(defun mprint-region (start end)
  "Print region contents with command given by `mprint-command'
 `mprint-switches' is a list of extra switches (strings) to pass to
 `mprint-command'."   
  (interactive "r")
  (mprint-region-1 start end (list mprint-switches 
				   ; (concat " | lpr -P" mprint-printer)
				   )))

(defun mprint-region-1 (start end switches)
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
     (message (concat "Spooling: " mprint-command " "
		      (mapconcat 'concat switches " ")))) 
     (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	  (set-buffer (get-buffer-create " *spool temp*"))
	  (widen) (erase-buffer)
	  (insert-buffer-substring oldbuf)
	  (setq tab-width width)
	  (untabify (point-min) (point-max))
  (setq start (point-min) end (point-max))))
     (apply 'call-process-region
	    (nconc (list start end
			 mprint-command
			 nil nil nil)
		   switches
		   ))
     (message "Spooling...done")))
