;;; emacs-wiki-journal.el --- Maintain a weblog style journal with emacs-wiki

;; Copyright 2003 Gary V. Vaughan (gary AT gnu DOT org)
;; This file has been hacked by Ole Arndt (ole AT sugarshark DOT com)

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-journal.el
;; Version: 0.3
;; Date: Fri, 2 May 2003
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Maintain weblog style journal in a local Emacs Wiki
;; URL: http://www.oranda.demon.co.uk/dist/emacs-wiki-journal.el
;; Compatibility: XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;_* Commentary

;; I maintain the hypertext parts of my website with John Wiegley's
;; emacs-wiki.el, now maintained by Damien Elmes at
;; http://repose.cx/emacs/wiki.  You will need to install a copy of that
;; file before this one is of any use to you.

;;;_ + Startup

;; To begin using emacs-wiki-journal, put this in your .emacs file:

;;   (load "emacs-wiki-journal")

;; Now you can add an entry to your journal with
;; M-x emacs-wiki-journal-add-entry, give it a WikiName for a category
;; to index the entry under (usually beginning "Category") and a header
;; line for the new entry.  A suitable header for today's entry will
;; added to emacs-wiki-journal-wiki where you add the body for the entry
;; and a cross reference will be added to the category index.  If you have
;; a PNG icon in the emacs-wiki-category-icons-url then the icon named
;; after the CategoryWiki will be inserted in the journal header.

;; You may need to set up a stylesheet to layout the journal page as you
;; want it.  Look at mine as an example:

;;   http://www.oranda.demon.co.uk/journal.css

;; You should also type M-x customize-group, and give the name
;; "emacs-wiki-journal".  Change it to suit your preferences.  Each of
;; the options has its own documentation.

;;;_ + Key Bindings

;; You might want to bind emacs-wiki-journal-add-entry to a key in your
;; global keymap:

;;   (define-key ctl-x-4-map
;;               "j" 'emacs-wiki-journal-add-entry-other-window)

;; And also in emacs-wiki mode:

;;   (add-hook 'emacs-wiki-mode-hook
;;             (lambda ()
;;               (local-set-key "C-cj" 'emacs-wiki-journal-add-entry)))


;;;_ + Code:

;; The parts of this code:
;;
;; * Customization group setup
;; * Category Index maintenance
;; * JournalWiki maintenance

;;;_ + Todo
;;
;; Only add category icons if the icon file exists
;; Search for category icon using emacs-wiki-image-regexp
;; Read in the CategoryWiki using emacs-wiki-read-name
;; Split the JournalWiki up by year and generate annual chronological indices

;;;_* Prerequisites
(require 'cl)
(require 'emacs-wiki)
(defvar emacs-wiki-journal-loaded nil)

;;;_* Options

(defun emacs-wiki-journal-option-customized (sym val)
  (set sym val)
  (when emacs-wiki-journal-loaded
    (emacs-wiki-journal-update-wiki-project)))

(defgroup emacs-wiki-journal nil
  "Options controlling the behaviour of Emacs Wiki journaling.
See `emacs-wiki-journal-add-entry' for more information."
  :group 'emacs-wiki)

(defcustom emacs-wiki-journal-directory "~/Blog"
  "The directory that contains your journal entries."
  :type 'directory
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-publishing-directory
  emacs-wiki-publishing-directory
  "The directory where all journal entries are published to."
  :type 'directory
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-wiki "MyJournal"
  "Default name of the file to which journal entries are added."
  :type 'string
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-default-category "CategoryMisc"
  "The default category used when adding entries."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-use-other-window nil
  "If non-nil, Emacs-Wiki journal will open in another window."
  :type 'boolean
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-icons-subdirectory "images"
  "Default base url to a directory containing category icons."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-time-format "%a, %e %b. %2y"
  "Format for the date string of journal entries.
See `format-time-string' for more information."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-category-regexp "^Category"
  "Each of the category index Wiki files start with this prefix."
  :type 'string
  :group 'emacs-wiki-journal)

;;;_* Internal Functions

(defvar emacs-wiki-journal-project "WikiJournal")

;; From planner.el
(defun emacs-wiki-journal-find-file (page &optional command)
  "Open the Emacs Wiki PAGE by name.
If COMMAND is non-nil, it is the function used to visit the file."
  (make-directory emacs-wiki-journal-directory t)
  (funcall (or command 'find-file)
	   (expand-file-name page emacs-wiki-journal-directory)))

(defun emacs-wiki-journal-category-alist (&optional no-check-p)
  "Return possible category index Wikis in `emacs-wiki-journal-directory'.
If NO-CHECK-P is non-nil, then don't check for changes in the directories
to decide whether to re-read the cached alist, just re-read the disk."
  (let ((file-alist 
	 (cadr (assoc emacs-wiki-journal-project emacs-wiki-file-alist)))
	(category-alist nil))
    (save-match-data
      (while file-alist
	(if (string-match emacs-wiki-journal-category-regexp
			  (caar file-alist))
	    (setq category-alist (cons (car file-alist)
				       category-alist)))
	(setq file-alist (cdr file-alist))))
    category-alist))

(defun emacs-wiki-journal-prompt-for-category-wiki ()
  "Prompt for a category index file."
  (let* ((directory emacs-wiki-journal-directory)
	 (file-alist (emacs-wiki-journal-category-alist))
	 (emacs-wiki-default-page emacs-wiki-journal-default-category))
    (emacs-wiki-read-name file-alist "Category: ")))

(defalias 'emacs-wiki-journal-make-link 'emacs-wiki-make-link)

(defun emacs-wiki-journal-add-category-entry (wiki target-url
					      &optional link-description)
  "Find category index file and add an entry for today."
  (emacs-wiki-journal-find-file wiki)
  (undo-boundary)
  (goto-char (point-min))

  ;; skip to first heading
  (goto-char 
   (or (search-forward-regexp "^\\* " (point-max) t) (point)))
  (beginning-of-line)
  (forward-line 1)

  (goto-char 
   (or (search-forward-regexp "^- " (point-max) t) (point)))
  (beginning-of-line)
  (insert (concat "- "
		  (format-time-string emacs-wiki-journal-time-format)
		  " "
		  (emacs-wiki-journal-make-link
		   target-url link-description)
		  "\n")))

(defun emacs-wiki-journal-1+-string (value)
  "Increment an ascii encoded number."
  (int-to-string (1+ (string-to-int value))))

(defun emacs-wiki-journal-update-wiki-project ()
  "Update the \"emacs-wiki-journal\" project in `emacs-wiki-projects"
  (setq emacs-wiki-projects
	(delq (assoc emacs-wiki-journal-project emacs-wiki-projects)
	      emacs-wiki-projects))
  (add-to-list 'emacs-wiki-projects
	       `(,emacs-wiki-journal-project .
		 ((emacs-wiki-directories .
					  (,emacs-wiki-journal-directory))
		  (emacs-wiki-default-page . ,emacs-wiki-journal-wiki)
		  (emacs-wiki-publishing-directory .
		  ,emacs-wiki-journal-publishing-directory))))
  (emacs-wiki-update-project-interwikis))

;;;_* User Functions

;;;###autoload
(defun emacs-wiki-journal-add (category-wiki journal-entry-heading)
  "Add a journal entry under category with a heading"
  (interactive)
  (let* ((category-anchor-base (downcase category-wiki))
	 (anchor-regexp (concat "^#" (regexp-quote category-anchor-base)
				"\\([0-9][0-9]*\\)"))
	 (anchor-ord "0"))

    (emacs-wiki-journal-find-file emacs-wiki-journal-wiki
				  (when emacs-wiki-journal-use-other-window
				    'find-file-other-window))

    (goto-char (point-min))
    (save-excursion
      (if (search-forward-regexp anchor-regexp (point-max) t)
	  (setq anchor-ord 
		(emacs-wiki-journal-1+-string
		 (buffer-substring (match-beginning 1)
				   (match-end 1))))))
    (save-excursion
      (emacs-wiki-journal-add-category-entry
       category-wiki
       (concat emacs-wiki-journal-wiki "#" category-anchor-base anchor-ord)
       journal-entry-heading))

    ;; skip # lines and blanks at the start of the buffer
    (while (and (looking-at "^\\(#\\|\n\\)")
		(equal 0 (forward-line 1))))

    ;; skip to first heading
    (goto-char 
     (or (search-forward-regexp "^\\* " (point-max) t) (point)))
    (beginning-of-line)

    (let* ((time-string (format-time-string
			 emacs-wiki-journal-time-format))
	   (icon-file-name (concat emacs-wiki-journal-icons-subdirectory "/"
				   category-wiki ".png"))
	   (icon-link (if (file-exists-p icon-file-name)
			  (concat (emacs-wiki-journal-make-link
			    icon-file-name) " ")
			"")))
      ;; only add a new date if different from the top entry
      (if (not (looking-at (regexp-quote (concat "* " time-string))))
	  (insert (concat "\n* " time-string "\n\n"))
	(forward-line 1)
	(insert "\n"))

      (open-line 1)
      (insert (concat "#" category-anchor-base anchor-ord " " icon-link "\n** " 
		      journal-entry-heading "\n*** " category-wiki "\n\n\n")))

    (emacs-wiki-journal-find-file emacs-wiki-journal-wiki)
    (forward-line -1)			; move to insertion point
    ))

;;;###autoload
(defun emacs-wiki-journal-add-entry ()
  "Find journal file and add an entry and category index for today."
  (interactive)
  (let ((category (emacs-wiki-journal-prompt-for-category-wiki))
	(heading (read-from-minibuffer "Journal Heading: ")))
    (emacs-wiki-journal-add category heading)))

;;;###autoload
(defun emacs-wiki-journal-add-entry-other-window ()
  "Find category index file in another window and add an entry for today."
  (interactive)
  (let ((emacs-wiki-journal-use-other-window t))
    (emacs-wiki-journal-add-entry)))

;;;_* Initialization

(setq emacs-wiki-journal-loaded t)
(emacs-wiki-journal-update-wiki-project)
(provide 'emacs-wiki-journal)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; emacs-wiki-journal.el ends here
