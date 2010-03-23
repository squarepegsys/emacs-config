;; random-sig.el - A random signature inserter for message-mode
;;
;; The function in this file will, given valid values in
;; `random-signature-head' and `random-signature-dir', build a random
;; signature for you.  `random-signature-head' is intended to be a static
;; file - you should put things that won't change between sigs in it
;; (your email address, URL, etc).  `random-signature-dir' is the name of
;; a directory that contains signature fragments to be appended to the
;; text contained in `random-signature-head'.  One of these fragments will
;; be picked at random.
;;
;; NOTE: `random-signature-head' CAN be placed in `random-signature-dir'.
;;       The function checks to make sure that the fragment that it has
;;       picked for appending is not `random-signature-head'.
;;
;; Please report bugs, make suggestions, etc to:
;;    Matt Simmons <simmonmt@acm.org>
;;    http://www.netcom.com/~simmonmt
;;
;; $Id: random-sig.el,v 1.1 1997/09/03 17:57:32 simmonmt Exp $
;;
;; $Log: random-sig.el,v $
;; Revision 1.1  1997/09/03 17:57:32  simmonmt
;; Initial revision
;;
;;

(defvar random-signature-head (expand-file-name "~/.signature")
  "*The file used as a sig header in `random-signature-fun'.
Set to nil if the header is not to be used")

(defvar random-signature-dir (expand-file-name "~/.sigs")
  "*The directory from which the random signature files are extracted")

(defun random-signature-fun ()
  "Insert a random signature.
The signature is built as follows:  `random-signature-head' is included, followed by a random file from `random-signature-dir'.
Note: A check is made to ensure that the random file picked from `random-signature-dir' is not `random-signature-head', so you can safely put `random-signature-head' in `random-signature-dir'."
  (interactive)

    ;; Set up message buffer for signature insertion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "\n-- \n")

    ;; Get the header (if necessary)
    (if random-signature-head
	(if (file-readable-p random-signature-head)
	    (insert-file-contents random-signature-head)
	    (insert-string (concat "*** Unable to insert header file '"
				   random-signature-head "' ***\n"))))

    ;; Get the random body
    (goto-char (point-max))
    (if (file-directory-p random-signature-dir)
	(let ((sig-files (directory-files random-signature-dir t nil nil t))
	      (sig-num nil))
	     (while (not sig-num)
	       (setq sig-num (random (length sig-files)))
	       (if (string-equal (nth sig-num sig-files) random-signature-head)
		   (setq sig-num nil)))
	     (insert-file-contents (nth sig-num sig-files)))
        (insert-string (concat "*** Unable to find body directory '"
			       random-signature-dir "' ***\n"))))

(provide 'random-sig)
