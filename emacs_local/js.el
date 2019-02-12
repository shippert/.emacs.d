;;; js-gnus-add.el --- Additional stuff for Gnus by Jonas Steverud

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: Jonas Steverud <d4jonas@dtek.chalmers.se>
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; In the beginning there where light. Some called this the Way of the
;; Nature and there was much rejoicing among them. Some look upon this
;; light and said "This looks good, but it's not The True Light(TM)."
;; and they invented and marked The True Light(TM) with questionable
;; methods. And darkness was upon the people of the land.

;; Those that had enjoyed the old light - and strangely enough didn't
;; enjoy The True Light(TM) - complained and invented a electric torch
;; which was free to use for everyone.

;; Much time went pass and for not so long ago there where an
;; ISO-standard for coding letters on electric machines - called
;; computers. Some called this Latin1 and there was much rejoicing
;; among them. Some look upon this Latin1 and said "This looks good,
;; but it's not The True Latin1(TM)." and they invented and marked The
;; True Latin1(TM) with questionable methods. And WinLatin1 was upon
;; the people of the land.

;; Those that had enjoyed the old Latin1 - and strangely enough didn't
;; enjoy The True Latin1(TM) - complained and invented a function
;; that would convert WinLatin1 into Latin1 and which was free to use
;; for everyone.

;; If you don't want to fight The True Light - light at least a
;; candle: <URL:http://www.amnesty.org/>.

;; To use the function, add this to your ~/.gnus:

;;(add-hook 'gnus-sum-load-hook
;;	  (function (lambda ()
;;		      (define-key gnus-summary-mode-map "WD"
;;			'js-gnus-make-MS-depreciation-to-Latin1-readable))))

;; or if you prefer do to it automatically:

;;(add-hook 'gnus-article-display-hook
;;	  'js-gnus-make-MS-depreciation-to-Latin1-readable)

;;; Code:

(provide 'js)

(defvar js-gnus-translationtable-for-MS-depreciation-to-Latin1
  '((?\202 . ",")     (?\203 . "f")
    (?\204 . ",,")    (?\213 . "<")
    (?\214 . "OE")    (?\205 . "...")
    (?\221 . "`")     (?\222 . "'")
    (?\223 . "``")    (?\224 . "''")
    (?\225 . "*")     (?\226 . "-")
    (?\227 . "-")     (?\231 . "(TM)")
    (?\233 . ">")     (?\234 . "oe")
    (?\264 . "'")  ; This is part of Latin1 but looks like a box on my screen.
    )
  "A table where the first value in an element is the code
for the character and the second is the string to substitute
it with.
<URL:http://czyborra.com/charsets/iso8859.html#ISO-8859-1>")

(defun js-gnus-make-MS-depreciation-to-Latin1-readable (&optional FORCE)
  "Translate WinLatin1 into Latin1 in the articlebuffer.
They call it a enhancement, we call it a depreciation.
If FORCE is non-nil the entire buffer is converted.
Uses js-gnus-translationtable-for-MS-depreciation-to-Latin1."
  
  (interactive "P")
  (save-excursion
    (set-buffer gnus-article-buffer)
    (goto-char (point-min))
    (when (or FORCE (search-forward "\n\n" nil t))
      ;; we are now standing at the beginning of the article-body.
      (let ((buffer-read-only nil)
	    (changes 0))
	(while (not (eobp))
	  (let ((xchar (assoc (following-char)
			      js-gnus-translationtable-for-MS-depreciation-to-Latin1))
		)
	    (if xchar		; If exchange character.
		(progn
		  (delete-char 1)
		  (insert (cdr xchar))
		  (incf changes) ; increase count.
		  ))
	    (forward-char)
	    ))
	(if (> changes 0)
	    (message (concat "Changed " (int-to-string changes)
			     " WinLatin1-characters to Latin1-characters.")))
	))))

;;; js-gnus-add.el ends here
