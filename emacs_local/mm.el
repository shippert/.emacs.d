;
; Try this nonsense out for RMAIL.
;

;(require 'transparent)

(defvar rmail-never-execute-automatically t 
        "*Prevent metamail from happening semi-automatically")

(defun rmail-check-content-type ()
  "Check for certain Content Type headers in mail"
  (rmail-maybe-execute-content-type nil))

(defun rmail-execute-content-type ()
  "Check for certain Content Type headers in mail"
  (interactive)
  (rmail-maybe-execute-content-type t))

(defun rmail-handle-content-type (ctype override dotoggle)
  (let (oldpt
        (oldbuf (current-buffer))
        (fname (make-temp-name "/tmp/rmailct")))
    
    (cond
     ((and rmail-never-execute-automatically (not override))
      (progn
       (if dotoggle (rmail-toggle-header))
       (message (concat "You can use '!' to run an interpreter for this '"
                        ctype "' format mail."))))
     ((or override
          (getenv "MM_NOASK")
          (y-or-n-p (concat "Run an interpreter for this '"
                            ctype "' format mail? ")))
      (progn
       (save-restriction
        (goto-char (point-max))
        (setq oldpt (point))
        (goto-char 0)
        (widen)
        (write-region
         (point)
         oldpt
         fname
         nil
         'nomessage))
       (if dotoggle (rmail-toggle-header))
       (if
        (and window-system (getenv "DISPLAY"))
         (progn
          (switch-to-buffer-other-window "METAMAIL")
          (erase-buffer)
          (pop-to-buffer oldbuf)
          (start-process "metamail"  "METAMAIL" "metamail" "-m"
                         "rmail" "-p" "-x" "-d" "-z" "-q" "-R" fname)
          (message "Starting metamail.  Sending output to METAMAIL buffer."))
         (progn
          (switch-to-buffer "METAMAIL")
          (erase-buffer)
          (sit-for 0)
          (transparent-window
           "METAMAIL"
           "metamail"
           (list "-m" "rmail" "-p" "-d" "-z" "-q" fname)
           nil
          (concat
           "\n\r\n\r*****************************************"
           "*******************************\n\rPress any key "
           "to go back to EMACS\n\r\n\r***********************" 
           "*************************************************\n\r")
	  )))))
     (t (progn
	  (if dotoggle (rmail-toggle-header))
	  (message (concat "You can use the '!' keystroke to "
			   "execute the external viewing program.")))))))

(defun rmail-maybe-execute-content-type (dorun)
  "Check for certain Content Type headers in mail"
  (cond
   ((not (getenv "NOMETAMAIL"))
    (let* ((ct nil)
           (needs-toggled nil))
      (save-excursion
       (save-restriction
        (widen)
        (goto-char (rmail-msgbeg rmail-current-message))
        (forward-line 1)
        (if (and dorun (= (following-char) ?1)) (setq needs-toggled t))
        (if (= (following-char) ?0)
          (narrow-to-region
           (progn (forward-line 2)
                  (point))
           (progn (search-forward "\n\n" (rmail-msgend rmail-current-message)
           'move)
           (point)))
         (narrow-to-region (point)
                           (progn (search-forward "\n*** EOOH ***\n")
                  (beginning-of-line) (point))))
        (setq ct (mail-fetch-field "content-type" t))))
      (cond
	(ct
	(cond ((and (not (string= (downcase ct) "text"))
       	     (not (string= (downcase ct) "text/plain"))
	            (not (string= (downcase ct)
       	                   "text/plain; charset=us-ascii")))
              (progn
               (if needs-toggled (rmail-toggle-header))
               (rmail-handle-content-type
                ct dorun needs-toggled)))
	     (needs-toggled
	      (rmail-toggle-header)))))))))

