;;; emacs-wiki.el --- Maintain a local Wiki using Emacs-friendly markup

;; Copyright (C) 2001, 2002, 2003, 2004 John Wiegley (johnw AT gnu DOT org)
;; Parts copyright (C) 2004 Sacha Chua (sacha AT free DOT net DOT ph)
;;
;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki.el
;; Version: 2004.08.07-03.08-stable
;; Keywords: hypermedia
;; Author: John Wiegley (johnw AT gnu DOT org)
;;         Alex Schroeder (alex AT gnu DOT org)
;; Maintainer: Sacha Chua <sacha@free.net.ph>
;; Description: Maintain Emacs-friendly Wikis in a local directory
;; Stable URL: http://sacha.free.net.ph/notebook/emacs/stable/emacs-wiki/
;; Dev URL: http://sacha.free.net.ph/notebook/emacs/dev/emacs-wiki/
;; Compatibility: Emacs20, Emacs21, XEmacs21

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

;;; Commentary:

;; Wiki is a concept, more than a thing.  It is a way of creating
;; document pages using plain text markup and simplified hyperlinking.

;; By typing a name in MixedCase, a hyperlink is automatically created
;; to the document "MixedCase".  Pressing return on that name will
;; create the file if it doesn't exist, or visit it if it does.

;; The markup used by emacs-wiki is intended to be very friendly to
;; people familiar with Emacs.  Type C-h v emacs-wiki-publishing-markup
;; after this mode is loaded for how to get started.

;; * Startup

;; To begin using emacs-wiki, put this in your .emacs file:

;;   (load "emacs-wiki")

;; Now you can type M-x emacs-wiki-find-file, give it a WikiName (or
;; just hit return) and start typing!

;; You should also type M-x customize-group, and give the name
;; "emacs-wiki".  Change it to suite your preferences.  Each of the
;; options has its own documentation.

;; * Keystroke summary

;; Here is a summary of keystrokes available in every Wiki buffer:

;;   C-c C-a    jump to an index of all the Wiki pages
;;   C-c C-b    show all pages that reference this page
;;   C-c C-s    search for a word in your Wiki pages
;;   C-c C-f    jump to another Wiki page; prompts for the name
;;   C-c C-l    highlight/refresh the current buffer
;;   C-c C-p    publish any Wiki pages that have changed as HTML
;;   C-c C-r    rename wiki link at point
;;   C-c C-v    change wiki project
;;   C-c C-D    delete wiki link at point (binding will only work on X)
;;   C-c =      diff this page against the last backup version
;;   TAB        move to the next Wiki reference
;;   S-TAB      move to the previous Wiki reference

;; * Using pcomplete

;; If you have pcomplete loaded, you can type M-TAB to complete Wiki
;; names.  Hitting M-TAB twice or more time in succession, will cycle
;; through all of the possibilities.  You can download pcomplete from
;; my Website:

;;   http://www.gci-net.com/~johnw/emacs.html

;; * ChangeLog support

;; If you use a ChangeLog (C-x 4 a) within one of your Wiki
;; directories, it will be used for notifying visitors to your wiki of
;; recent changes.

;; * Changing title or stylesheet

;; For convenience, if you want to change the visible title, or the
;; stylesheet, used by a certain Wiki page during HTML publishing,
;; just put:

;; #title Hello there
;; #style hello.css

;; at the top of the page.

;; * <lisp> tricks

;; <lisp></lisp> tags can be used, not only to evaluate forms for
;; insertion at that point, but to influence the publishing process in
;; many ways.  Here's another way to change a page's stylesheet:

;; <lisp>
;; (ignore
;;   ;; use special.css for this Wiki page
;;   (set (make-variable-buffer-local 'emacs-wiki-style-sheet)
;;        "<link rel=\"stylesheet\" type=\"text/css\" href=\"special.css\" />"))
;; </lisp>

;; The 'ignore' is needed so nothing is inserted where the <lisp> tag
;; occurred.  Also, there should be no blank lines before or after the
;; tag (to avoid empty paragraphs from being created).  The best place
;; to put this would be at the very top or bottom of the page.

;; * Sub-lists?

;; There is no inherent support for sub-lists, since I couldn't think
;; of a simple way to do it.  But if you really need them, here's a
;; trick you can use:

;; - Hello
;;   <ul>
;;   <li>There
;;   <li>My friend
;;   </ul>

;;; Thanks

;; Alex Schroeder (alex AT gnu DOT org), current author of "wiki.el".
;;   His latest version is here:
;;       http://www.geocities.com/kensanata/wiki/WikiMode.html
;;
;; Frank Gerhardt (Frank.Gerhardt AT web DOT de), author of the original wiki-mode
;;   His latest version is here:
;;       http://www.s.netic.de/fg/wiki-mode/wiki.el
;;
;; Thomas Link (<t.link AT gmx DOT at)

;;; Code:

;; The parts of this code, and work to be done:
;;
;; * setup emacs-wiki major mode
;; * generate WikiName list
;; * utility functions to extract link parts
;; * open a page
;; * navigate links in the buffer
;; * visit a link
;; * search Wiki pages for text/backlinks
;; * index generation
;; * buffer highlighting (using font-lock)
;; * HTML publishing
;;   - Allow for alternate markup tables: DocBook, xhtml, etc.
;;   - <nop> used in a line of verse doesn't have effect
;; * HTTP serving (using httpd.el)
;;   - Diffing (look at using highlight-changes-mode and htmlify.el)
;;   - Editing (requires implementing POST method for httpd.el)

(defvar emacs-wiki-version "2004.08.07-03.08-stable"
  "The version of emacs-wiki currently loaded")

(require 'derived)
(require 'font-lock)

;; load pcomplete if it's available
(load "pcomplete" t t)

(defvar emacs-wiki-under-windows-p (memq system-type '(ms-dos windows-nt)))

;;; Options:

(defgroup emacs-wiki nil
  "Options controlling the behaviour of Emacs Wiki Mode.
Wiki is a concept, more than a thing.  It is a way of creating
document pages using plain text markup and simplified hyperlinking.

By typing a name in MixedCase, a hyperlink is automatically created
to the document \"MixedCase\".  Pressing return on that name will
create the file if it doesn't exist, or visit it if it does.

The markup used by emacs-wiki is intended to be very friendly to
people familiar with Emacs.  See the documentation for the variable
`emacs-wiki-publishing-markup' for a full description."
  :group 'hypermedia)

(defcustom emacs-wiki-mode-hook
  '(emacs-wiki-use-font-lock)
  "A hook that is run when emacs-wiki mode is entered."
  :type 'hook
  :options '(emacs-wiki-use-font-lock
             emacs-wiki-highlight-buffer
             flyspell-mode
             footnote-mode
             highlight-changes-mode)
  :group 'emacs-wiki)

;;;###autoload
(defcustom emacs-wiki-directories '("~/Wiki")
  "A list of directories where Wiki pages can be found."
  :require 'emacs-wiki
  :type '(repeat :tag "Wiki directories" directory)
  :group 'emacs-wiki)

(defcustom emacs-wiki-default-page "WelcomePage"
  "Name of the default page used by \\[emacs-wiki-find-file]."
  :type 'string
  :group 'emacs-wiki)

(defcustom emacs-wiki-default-project "default"
  "Name of the project to use by default. Used by
  \\[emacs-wiki-change-project] to determine a default
project to switch to."
  :type 'string
  :group 'emacs-wiki)

(defcustom emacs-wiki-file-ignore-regexp
  "\\`\\(\\.?#.*\\|.*,v\\|.*~\\|\\.\\.?\\)\\'"
  "A regexp matching files to be ignored in Wiki directories."
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-ignored-extensions-regexp
  "\\.\\(bz2\\|gz\\|[Zz]\\)\\'"
  "A regexp of extensions to omit from the ending of Wiki page name."
  :type 'string
  :group 'emacs-wiki)

(defcustom emacs-wiki-interwiki-names
  '(("GnuEmacs" . "http://www.gnu.org/software/emacs/emacs.html")
    ("TheEmacsWiki" .
     (lambda (tag)
       (concat "http://www.emacswiki.org/cgi-bin/wiki.pl?"
               (or tag "SiteMap"))))
    ("MeatballWiki" .
     (lambda (tag)
       (concat "http://www.usemod.com/cgi-bin/mb.pl?"
               (or tag "MeatballWiki")))))
  "A table of WikiNames that refer to external entities.
The format of this table is an alist, or series of cons cells.
Each cons cell must be of the form:

  (WIKINAME . STRING-OR-FUNCTION)

The second part of the cons cell may either be a STRING, which in most
cases should be a URL, or a FUNCTION.  If a function, it will be
called with one argument: the tag applied to the Interwiki name, or
nil if no tag was used.  If the cdr was a STRING and a tag is used,
the tag is simply appended.

Here are some examples:

  (\"JohnWiki\" . \"http://alice.dynodns.net/wiki?\")

Referring to [[JohnWiki#EmacsModules]] then really means:

  http://alice.dynodns.net/wiki?EmacsModules

If a function is used for the replacement text, you can get creative
depending on what the tag is.  Tags may contain any alphabetic
character, any number, % or _.  If you need other special characters,
use % to specify the hex code, as in %2E.  All browsers should support
this."
  :type '(repeat (cons (string :tag "WikiName")
                       (choice (string :tag "URL") function)))
  :group 'emacs-wiki)

(defvar emacs-wiki-url-or-name-regexp nil
  "Matches either a Wiki link or a URL.  This variable is auto-generated.")

(defvar emacs-wiki-url-or-name-regexp-group-count nil
  "Matches either a Wiki link or a URL.  This variable is auto-generated.")

(defcustom emacs-wiki-extended-link-regexp
  "\\[\\[\\([^][\n]+\\)\\]\\(\\[\\([^][\n]+\\)\\]\\)?\\]"
  "Regexp used to match [[extended][links]]."
  :type 'regexp
  :group 'emacs-wiki)

(defun emacs-wiki-count-chars (string char)
  (let ((i 0)
        (l (length string))
        (count 0))
    (while (< i l)
      (if (eq char (aref string i))
          (setq count (1+ count)))
      (setq i (1+ i)))
    count))

(defun emacs-wiki-set-sym-and-url-regexp (sym value)
  (when (eq sym 'emacs-wiki-url-protocols)
    (setq emacs-wiki-url-protocols value)
    (setq emacs-wiki-url-regexp
          (concat "\\<\\("
                  (mapconcat 'car emacs-wiki-url-protocols "\\|")
                  "\\):"
                  "[^]  \n \"'()<>[^`{}]*[^]    \n \"'()<>[^`{}.,;]+")))
  (setq emacs-wiki-url-or-name-regexp
        (concat "\\("
                (if (eq sym 'emacs-wiki-name-regexp)
                    value
                  emacs-wiki-name-regexp) "\\|"
                  (if (eq sym 'emacs-wiki-name-regexp)
                      (if (boundp 'emacs-wiki-url-regexp)
                          emacs-wiki-url-regexp
                        "")
                    emacs-wiki-url-regexp) "\\)")
        emacs-wiki-url-or-name-regexp-group-count
        (- (emacs-wiki-count-chars
            emacs-wiki-url-or-name-regexp ?\() 2))
  (set sym value))

(defcustom emacs-wiki-name-regexp
  (concat "\\(" emacs-wiki-extended-link-regexp "\\|"
          "\\<[A-Z][a-z]+\\([A-Z][a-z]+\\)+\\(#[A-Za-z0-9]+\\)?" "\\)")
  "Regexp used to match WikiNames."
  :type 'regexp
  :set 'emacs-wiki-set-sym-and-url-regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-url-protocols
  '(("info" emacs-wiki-browse-url-info nil)
    ("man" emacs-wiki-browse-url-man nil)
    ("google" emacs-wiki-browse-url-google emacs-wiki-resolve-url-google)
    ("http" browse-url identity)
    ("https" browse-url identity)
    ("ftp" browse-url identity)
    ("gopher" browse-url identity)
    ("telnet" browse-url identity)
    ("wais" browse-url identity)
    ("file" browse-url identity)
    ("news" browse-url identity)
    ("snews" browse-url identity)
    ("mailto" browse-url identity))
  "A list of (PROTOCOL BROWSE-FUN MARKUP-FUN) used to match protocols for URLs.
BROWSE-FUN should accept URL as an argument and open the URL in the
current window. MARKUP-FUN should accept URL as an argument and return
the final URL or nil if no URL should be included."
  :type '(alist
          :key-type (string :format "Protocol: %v")
          :value-type (group (function :format "Browse: %v")
                             (function :format "Resolve: %v")))
  :set 'emacs-wiki-set-sym-and-url-regexp
  :group 'emacs-wiki)

(defvar emacs-wiki-url-regexp
  nil
  "A regexp used to match URLs within a Wiki buffer.
Dynamically calculated from `emacs-wiki-url-protocols'.")

(defcustom emacs-wiki-grep-command
  "find %D -type f ! -name '*~' | xargs egrep -n -e \"\\<%W\\>\""
  "The name of the program to use when grepping for backlinks.
The string %D is replaced by `emacs-wiki-directories', space-separated.
The string %W is replaced with the name of the Wiki page.

Note: I highly recommend using glimpse to search large Wikis.  To use
glimpse, install and edit a file called .glimpse_exclude in your home
directory.  Put a list of glob patterns in that file to exclude Emacs
backup files, etc.  Then, run the indexer using:

  glimpseindex -o <list of Wiki directories>

Once that's completed, customize this variable to have the following
value:

  glimpse -nyi \"%W\"

Your searches will go much, much faster, especially for very large
Wikis.  Don't forget to add a user cronjob to update the index at
intervals."
  :type 'string
  :group 'emacs-wiki)

(defun emacs-wiki-edit-link-at-point ()
  "Edit the current link.
Do not rename the wiki page originally referred to."
  (interactive "*")
  (if (emacs-wiki-link-at-point)
      (let* ((old-link (match-string-no-properties 2))
             (old-text (match-string-no-properties 4))
             (match-start (match-beginning 0))
             (match-end (match-end 0))
             (link (planner-make-link
                    (read-string "Link: "
                                 (emacs-wiki-link-unescape old-link))
                    (read-string "Text: "
                                 (emacs-wiki-link-unescape old-text t)))))
        (delete-region match-start match-end)
        (insert link))
    (error "There is no valid link at point")))

(defvar emacs-wiki-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?c) (control ?a)] 'emacs-wiki-index)
    (define-key map [(control ?c) (control ?f)] 'emacs-wiki-find-file)
    (define-key map [(control ?c) (control ?b)] 'emacs-wiki-backlink)
    (define-key map [(control ?c) (control ?s)] 'emacs-wiki-search)
    (define-key map [(control ?c) (control ?p)] 'emacs-wiki-publish)
    (define-key map [(control ?c) (control ?v)] 'emacs-wiki-change-project)
    (define-key map [(control ?c) (control ?e)] 'emacs-wiki-edit-link-at-point)
    (define-key map [(control ?c) (control ?r)]
                                          'emacs-wiki-rename-link-at-point)
    (define-key map [(control ?c) (control ?D)]
                                          'emacs-wiki-delete-link-at-point)

    (define-key map [(control ?c) (control ?l)] 'font-lock-mode)

    (define-key map [(control ?c) ?=]
      (lambda ()
        (interactive)
        (diff-backup buffer-file-name)))

    (define-key map [tab] 'emacs-wiki-next-reference)
    (define-key map [(control ?i)] 'emacs-wiki-next-reference)

    (define-key map [(shift tab)] 'emacs-wiki-previous-reference)
    (unless (featurep 'xemacs)
      (define-key map [(shift iso-lefttab)] 'emacs-wiki-previous-reference)
      (define-key map [(shift control ?i)] 'emacs-wiki-previous-reference))

    (when (featurep 'pcomplete)
      (define-key map [(meta tab)] 'pcomplete)
      (define-key map [(meta control ?i)] 'pcomplete))

    map)
  "Keymap used by Emacs Wiki mode.")

(defvar emacs-wiki-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] 'emacs-wiki-follow-name-at-point)
    (define-key map [(control ?m)] 'emacs-wiki-follow-name-at-point)
    (define-key map [(shift return)] 'emacs-wiki-follow-name-at-point-other-window)
    (if (featurep 'xemacs)
        (progn
          (define-key map [(button2)] 'emacs-wiki-follow-name-at-mouse)
          (define-key map [(shift button2)] 'emacs-wiki-follow-name-at-mouse-other-window))
      (define-key map [(shift control ?m)] 'emacs-wiki-follow-name-at-point-other-window)
      (define-key map [mouse-2] 'emacs-wiki-follow-name-at-mouse)
      (define-key map [(shift mouse-2)] 'emacs-wiki-follow-name-at-mouse-other-window)
      (unless (eq emacs-major-version 21)
        (set-keymap-parent map emacs-wiki-mode-map)))
    map)
  "Local keymap used by emacs-wiki while on a WikiName.")

;; Code:

(defvar emacs-wiki-project nil)

;;;###autoload
(define-derived-mode emacs-wiki-mode text-mode "Wiki"
  "An Emacs mode for maintaining a local Wiki database.

Wiki is a hypertext and a content management system: Normal users are
encouraged to enhance the hypertext by editing and refactoring existing
wikis and by adding more.  This is made easy by requiring a certain way
of writing the wikis.  It is not as complicated as a markup language
such as HTML.  The general idea is to write plain ASCII.

Words with mixed case such as ThisOne are WikiNames.  WikiNames are
links you can follow.  If a wiki with that name exists, you will be
taken there.  If such a does not exist, following the link will create
a new wiki for you to fill.  WikiNames for non-existing wikis are
rendered as links with class \"nonexistent\", and are also displayed
in a warning color so that you can see whether following the link will
lead you anywhere or not.

In order to follow a link, hit RET when point is on the link, or use
mouse-2.

All wikis reside in the `emacs-wiki-directories'.

\\{emacs-wiki-mode-map}"
  (emacs-wiki-change-project (or emacs-wiki-project
                                 emacs-wiki-default-project))
  ;; because we're not inheriting from normal-mode, we need to
  ;; explicitly run file variables if the user wants to
  (condition-case err
      (hack-local-variables)
    (error (message "File local-variables error: %s"
                    (prin1-to-string err))))
  ;; bootstrap the file-alist, if it's not been read in yet
  (emacs-wiki-file-alist)
  ;; if pcomplete is available, set it up!
  (when (featurep 'pcomplete)
    (set (make-variable-buffer-local 'pcomplete-default-completion-function)
         'emacs-wiki-completions)
    (set (make-variable-buffer-local 'pcomplete-command-completion-function)
         'emacs-wiki-completions)
    (set (make-variable-buffer-local 'pcomplete-parse-arguments-function)
         'emacs-wiki-current-word))
  ;; avoid problems caused by emacs-wiki-url-protocols not getting
  ;; custom-set properly.
  (emacs-wiki-set-sym-and-url-regexp 'emacs-wiki-url-protocols
                                     emacs-wiki-url-protocols)
  ;; avoid problems caused by the 'intangible' text property with paragraph
  ;; filling.
  (set (make-local-variable 'inhibit-point-motion-hooks) t))

(defsubst emacs-wiki-page-file (page &optional no-check-p)
  "Return a filename if PAGE exists within the current Wiki."
  (cdr (assoc page (emacs-wiki-file-alist no-check-p))))

(defun emacs-wiki-directories-member (&optional directories)
  "Return non-nil if the current buffer is in `emacs-wiki-directories'."
  (unless (null buffer-file-name)
    (let ((here (expand-file-name (file-name-directory buffer-file-name)))
          (d (or directories emacs-wiki-directories))
          yes)
      (while d
        (let ((dir (file-name-as-directory (if (consp (car d))
                                               (caar d) (car d)))))
          (if (and (string-match
                    (concat "^" (regexp-quote (expand-file-name dir))) here)
                   ;; not an ignored file
                   (not (string-match
                         emacs-wiki-file-ignore-regexp buffer-file-name)))
              (setq yes (car d) d nil)
            (setq d (cdr d)))))
      yes)))

(defun emacs-wiki-maybe (&optional check-only)
  "Maybe turn Emacs Wiki mode on for this file."
  (let ((projs emacs-wiki-projects)
        (mode-func 'emacs-wiki-mode)
        project yes)
    (while (and (not yes) projs)
      (let* ((projsyms (cdar projs))
             (pred (assq 'emacs-wiki-predicate projsyms))
             dirs)
        (if pred
            (setq yes (funcall (cdr pred)))
          (setq dirs (assq 'emacs-wiki-directories projsyms))
          (if dirs
              (setq yes (emacs-wiki-directories-member (cdr dirs)))))
        (if yes
            (setq project (caar projs)
                  mode-func (or (cdr (assq 'emacs-wiki-major-mode projsyms))
                                mode-func))))
      (setq projs (cdr projs)))
    (setq yes (or yes (emacs-wiki-directories-member)))
    (if (and yes (not check-only))
        (let ((emacs-wiki-project project))
          (funcall mode-func)))
    yes))

(add-hook 'find-file-hooks 'emacs-wiki-maybe)

;;; Support WikiName completion using pcomplete

(defun emacs-wiki-completions ()
  "Return a list of possible completions names for this buffer."
  (while (pcomplete-here
          (mapcar 'car (append (emacs-wiki-file-alist)
                               emacs-wiki-interwiki-names)))))

(defun emacs-wiki-current-word ()
  (let ((end (point)))
    (save-restriction
      (save-excursion
        (skip-chars-backward "^\\[ \t\n")
        (narrow-to-region (point) end))
      (pcomplete-parse-buffer-arguments))))

;;; Return an list of known wiki names and the files they represent.

(defsubst emacs-wiki-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun emacs-wiki-page-name (&optional name)
  "Return the canonical form of the Wiki page name.
All this means is that certain extensions, like .gz, are removed."
  (save-match-data
    (unless name
      (setq name buffer-file-name))
    (if name
        (let ((page (file-name-nondirectory name)))
          (if (string-match emacs-wiki-ignored-extensions-regexp page)
              (replace-match "" t t page)
            page)))))

(defun emacs-wiki-page-title (&optional name)
  "Return the canonical form of the Wiki page name.
All this means is that certain extensions, like .gz, are removed."
  (or emacs-wiki-current-page-title
      (emacs-wiki-prettify-title (emacs-wiki-page-name name))))

(defun emacs-wiki-page-date ()
  "Return this page's publishing date."
  (or emacs-wiki-current-page-date
      (format-time-string "%e %B %Y"
                          (nth 5 (file-attributes buffer-file-name)))))

(defun emacs-wiki-walk-directories (directories)
  "Return a list of subdirectories in DIRECTORIES, excluding hidden dirs.
Directory paths are expanded to avoid duplication. You can use
this when initializing `emacs-wiki-directories'."
  (let (d)
    (while directories
      (let ((pending-dir (expand-file-name (car directories))))
        (and (file-directory-p (car directories))
             (not (member pending-dir d))
             (setq d (append d (list (car directories)))
                   directories (append directories
                                       (directory-files
                                        (car directories) t "^[^.]" t)))))
      (setq directories (cdr directories)))
    d))

(defvar emacs-wiki-file-alist nil)

(defcustom emacs-wiki-refresh-file-alist-p
  nil
  "Function that returns non-nil if the file list needs to be refreshed.
If nil, never refresh. If t, always refresh. See
`emacs-wiki-refresh-file-alist-maybe'."
  :type '(choice
          (const :tag "Do not automatically refresh" nil)
          (const :tag "Always automatically refresh" t)
          (function :tag "Function"))
  :group 'emacs-wiki)

(defun emacs-wiki-refresh-file-alist-maybe ()
  "Return non-nil if any of the files have been modified.
Suitable for use as `emacs-wiki-refresh-file-alist-p'."
  (let* ((file-alist (assoc emacs-wiki-current-project
                            emacs-wiki-file-alist))
         (d emacs-wiki-directories) last-mod)
    ;; Set `last-mod' to modification time of last modified directory.
    (while d
      (let ((mod-time (nth 5 (file-attributes (car d)))))
        (if (or (null last-mod)
                (and mod-time (emacs-wiki-time-less-p last-mod mod-time)))
            (setq last-mod mod-time)))
      (setq d (cdr d)))
    (or (null (cddr file-alist))
        (null last-mod)
        (emacs-wiki-time-less-p (cddr file-alist)
                                last-mod))))

(defun emacs-wiki-refresh-file-alist ()
  "Force a refresh of the file alist."
  (interactive)
  (let* ((file-alist (assoc emacs-wiki-current-project
                            emacs-wiki-file-alist))
         (d emacs-wiki-directories) last-mod)
    ;; Store the current `last-mod' against the `emacs-wiki-current-project'
    ;; assoc key in `emacs-wiki-file-alist' to timestamp this recalculation
    ;; of the list of files.
    (while d
      (let ((mod-time (nth 5 (file-attributes (car d)))))
        (if (or (null last-mod)
                (and mod-time (emacs-wiki-time-less-p last-mod mod-time)))
            (setq last-mod mod-time)))
      (setq d (cdr d)))
    (if file-alist
        (setcdr (cdr file-alist) last-mod)
      (setq file-alist (cons emacs-wiki-current-project (cons nil last-mod))
            emacs-wiki-file-alist (cons file-alist emacs-wiki-file-alist)))
    ;; Add the current list of subdirectories to `emacs-wiki-directories'.
    ;; If a new file or directory is added to a directory in this list,
    ;; the modification time of one of these directories will change,
    ;; the `last-mod' checks above will notice, and the files and/or
    ;; directories that triggered the change will be added below.
    (setq emacs-wiki-directories
          (emacs-wiki-walk-directories emacs-wiki-directories))
    ;; Recalculate the list of files.
    (save-match-data
      (setcar
       (cdr file-alist)
       (let* ((dirs emacs-wiki-directories)
              (names (list t))
              (lnames names))
         (while dirs
           (if (file-readable-p (car dirs))
               (let ((files (directory-files (car dirs) t nil t)))
                 (while files
                   (unless
                       (or (file-directory-p (car files))
                           (string-match emacs-wiki-file-ignore-regexp
                                         (file-name-nondirectory
                                          (car files))))
                     (setcdr lnames
                             (cons (cons (emacs-wiki-page-name (car files))
                                         (car files)) nil))
                     (setq lnames (cdr lnames)))
                   (setq files (cdr files)))))
           (setq dirs (cdr dirs)))
         (cdr names))))))

(defun emacs-wiki-file-alist (&optional no-check-p)
  "Return possible Wiki filenames in `emacs-wiki-directories'."
  (let ((file-list
         (cadr (assoc emacs-wiki-current-project emacs-wiki-file-alist))))
    (when (or (null file-list)
              no-check-p
              (if (functionp emacs-wiki-refresh-file-alist-p)
                  (funcall emacs-wiki-refresh-file-alist-p)
                emacs-wiki-refresh-file-alist-p))
      (emacs-wiki-refresh-file-alist)
      (setq file-list (cadr (assoc emacs-wiki-current-project
                                   emacs-wiki-file-alist))))
    file-list))

(defun emacs-wiki-complete-alist ()
  "Return equivalent of calling (emacs-wiki-file-alist) for all projects."
  (let ((emacs-wiki-current-project "_CompositeFileList")
        (emacs-wiki-directories
         (copy-alist emacs-wiki-directories))
        (projs emacs-wiki-projects))
    (while projs
      (let* ((projsyms (cdar projs))
             (dirs (cdr (assq 'emacs-wiki-directories projsyms))))
        (while dirs
          (add-to-list 'emacs-wiki-directories (car dirs))
          (setq dirs (cdr dirs))))
      (setq projs (cdr projs)))
    (emacs-wiki-file-alist)))

;; Utility functions to extract parts of a Wiki name

(defvar emacs-wiki-serving-p nil
  "Non-nil when emacs-wiki is serving a wiki page directly.")

(defsubst emacs-wiki-transform-name (name)
  "Transform NAME as per `emacs-wiki-publishing-transforms', returning NAME"
  (save-match-data
    (mapc (function
           (lambda (elt)
             (let ((reg (car elt))
                   (rep (cdr elt)))
               (setq name
                     (emacs-wiki-replace-regexp-in-string
                      reg rep name t)))))
          emacs-wiki-publishing-transforms)
    name))

(defsubst emacs-wiki-published-name (name &optional current)
  "Return the externally visible NAME for a wiki page, possibly transformed
  via `emacs-wiki-publishing-transforms'. If CURRENT is provided, convert any
  path to be relative to it"
  (emacs-wiki-transform-name
   (progn
     (when current
       (setq name (file-relative-name name
                                      (file-name-directory
                                       (emacs-wiki-transform-name current)))))
     (concat (if emacs-wiki-serving-p
                 (unless (string-match "\\?" name) "wiki?")
               emacs-wiki-publishing-file-prefix)
             name
             (if emacs-wiki-serving-p
                 (if emacs-wiki-current-project
                     (concat "&project=" emacs-wiki-current-project))
               emacs-wiki-publishing-file-suffix)))))

(defsubst emacs-wiki-published-file (&optional file)
  "Return the filename of the published file. Since this is based on the
  published-name, it will be filtered through
  `emacs-wiki-publishing-transforms'"
  (expand-file-name
   (emacs-wiki-published-name (emacs-wiki-page-name file))
   (let ((d emacs-wiki-directories)
         (alist (emacs-wiki-file-alist))
         found)
     (while d
       (let* ((dir (file-name-as-directory (if (consp (car d))
                                               (caar d) (car d))))
              (dir-re (concat "^" (regexp-quote (expand-file-name dir))))
              (buffer-file (or (cdr (assoc file alist)) buffer-file-name))
              (buffer-directory
               (file-name-directory
                (or buffer-file emacs-wiki-publishing-directory))))
         (if (string-match dir-re buffer-directory)
             (setq found (substring buffer-directory (match-end 0)) d nil)
           (setq d (cdr d)))))
     (if found
         (concat (file-name-as-directory emacs-wiki-publishing-directory)
                 found)
       emacs-wiki-publishing-directory))))

(defcustom emacs-wiki-publishing-transforms nil
  "A list of cons cells mapping regexps to replacements, which is applied when
generating the published name from the wiki file name. The replacements
run in order so you can chain them together.

An example is how I publish the emacs-wiki documentation. The emacs-wiki
homepage is in a file called EmacsWiki. With the following settings I can
publish directly to my webserver via tramp (the first rule catches 'WikiMarkup'
for instance):

(setq emacs-wiki-publishing-directory \"/webserver:/var/www/\")
(setq emacs-wiki-publishing-transforms
      '((\".*Wiki.*\" . \"emacs/wiki/\\\&\")
         (\"EmacsWiki\\|WelcomePage\" . \"index\")))

Then when trying to publish a page EmacsWiki:

(emacs-wiki-published-file \"EmacsWiki\")

You get:

\"/webserver:/var/www/emacs/wiki/index.html\""
  :type '(repeat
          (cons
           (regexp :tag "String to match")
           (string :tag "Replacement string")))
  :group 'emacs-wiki-publish)

(defsubst emacs-wiki-wiki-url-p (name)
  "Return non-nil if NAME is a URL."
  (save-match-data
    (string-match emacs-wiki-url-regexp name)))

(defun emacs-wiki-wiki-visible-name (wiki-name)
  "Return the visible part of a Wiki link.
This only really means something if [[extended][links]] are involved."
  (save-match-data
    (let ((name wiki-name))
      (if (string-match emacs-wiki-extended-link-regexp name)
          (if (match-string 2 name)
              (setq name (match-string 3 name))
            (setq name (match-string 1 name))
            (when (string-match "^mailto:" name)
              (setq name (substring name (match-end 0))))))
      (if (and (not (emacs-wiki-wiki-url-p name))
               (string-match "#" name))
          (if (= 0 (match-beginning 0))
              (setq name (emacs-wiki-page-name))
            (let ((base (substring name 0 (match-beginning 0))))
              (if (assoc base emacs-wiki-interwiki-names)
                  (setq name (concat (substring name 0 (match-beginning 0))
                                     ":" (substring name (match-end 0))))
                (setq name base)))))
      (emacs-wiki-link-unescape name t))))

(defun emacs-wiki-wiki-tag (wiki-name)
  (save-match-data
    (if (string-match "#" wiki-name)
        (substring wiki-name (match-end 0)))))

(defun emacs-wiki-replace-regexp-in-string (regexp replacement text
                                                   &optional substitute)
  "Replace REGEXP with REPLACEMENT in TEXT.
If SUBSTITUTE is non-nil, use `replace-match'-style substitutions."
  ;; This inversion is necessary because old code had been written to
  ;; always literally replace.
  (cond
   ((fboundp 'replace-in-string)
    (let ((case-fold-search nil))
      (replace-in-string text regexp replacement (not substitute))))
   ((fboundp 'replace-regexp-in-string)
    (let ((case-fold-search nil))
      (replace-regexp-in-string regexp replacement text t (not substitute))))
   (t (while (string-match regexp text)
        (setq text (replace-match replacement t (not substitute) text)))
      text)))

;; FIXME: Find a more efficient way to do this.
(defun emacs-wiki-link-escape (text &optional further)
  "Escape dangerous characters in TEXT."
  (when text
    (save-match-data
      (emacs-wiki-replace-regexp-in-string
       "\\[" "%5B"
       (emacs-wiki-replace-regexp-in-string
        "\\]" "%5D"
        (if further
            (emacs-wiki-replace-regexp-in-string
             "#" "%23" text)
          text))))))

(defun emacs-wiki-link-unescape (text &optional further)
  "Unescape dangerous characters in TEXT.
If FURTHER is non-nil, escape more characters that are
potentially dangerous."
  (when text
    (save-match-data
      (emacs-wiki-replace-regexp-in-string
       "%5B" "["
       (emacs-wiki-replace-regexp-in-string
        "%5D" "]"
        (if further
            (emacs-wiki-replace-regexp-in-string
             "%23" "#" text)
          text))))))

(defun emacs-wiki-make-link (link &optional name)
  "Return a Wiki link to LINK with NAME as the text."
  (let ((case-fold-search nil))
    (unless name
      (setq name (emacs-wiki-wiki-visible-name link)))
    (setq link (emacs-wiki-wiki-link-target link))
    (when (equal name link)
      (setq name nil))
    (if name
        (if (and (string-match
                  (concat "^\\(?:" emacs-wiki-url-or-name-regexp "\\)$") link)
                 (string= name link))
            name
          (concat "[[" (emacs-wiki-link-escape link)
                  "][" (emacs-wiki-link-escape name t) "]]"))
      ;; No name
      (if (string-match
           (concat "^\\(?:" emacs-wiki-url-or-name-regexp "\\)$") link)
          link
        (concat "[[" (emacs-wiki-link-escape link) "]]")))))

(defvar emacs-wiki-bare-digits-anchor-prefix "note"
  "Prefix to add to anchors composed only of digits.")

(defun emacs-wiki-wiki-link-target (wiki-name)
  "Return the target of a Wiki link.  This might include anchor tags."
  (save-match-data
    (let ((name wiki-name) lookup)
      (if (string-match "^\\[\\[\\([^]]+\\)\\]" name)
          (setq name (match-string 1 name)))
      (unless (emacs-wiki-page-file name)
	(if (and emacs-wiki-interwiki-names
		 (string-match "\\`\\([^#]+\\)\\(#\\(.+\\)\\)?\\'" name)
		 (setq lookup (assoc (match-string 1 name)
				     emacs-wiki-interwiki-names)))
	    (let ((tag (match-string 3 name))
		  (target (cdr lookup)))
	      (if (stringp target)
		  (setq name (concat target tag))
		(setq name (funcall target tag))))
	  (if (and (> (length name) 0)
		   (eq (aref name 0) ?#))
	      (setq name (concat (emacs-wiki-page-name) name)))
	  ;; Resolve numeric anchors to emacs-wiki-bare-digits-anchor-prefix
	  (when (string-match "#\\([0-9]+\\)" name)
	    (setq name (replace-match
			(concat "#" emacs-wiki-bare-digits-anchor-prefix
				(match-string 1 name))
			t t name)))))
      (emacs-wiki-link-unescape name))))

(defun emacs-wiki-wiki-base (wiki-name)
  "Find the WikiName or URL mentioned by a Wiki link.
This means without tags, in the case of a WikiName."
  (save-match-data
    (let ((file (emacs-wiki-wiki-link-target wiki-name)))
      (when file
        (if (emacs-wiki-wiki-url-p file)
            file
          (if (string-match "#" file)
              (substring file 0 (match-beginning 0))
            file))))))

;;; Open a Wiki page (with completion)

(defvar emacs-wiki-history-list nil)

(defun emacs-wiki-read-name (file-alist &optional prompt)
  "Read the name of a valid Wiki page from minibuffer, with completion."
  (let* ((default emacs-wiki-default-page)
         (str (completing-read
               (format "%s(default: %s) " (or prompt "Wiki page: ") default)
               file-alist nil nil nil 'emacs-wiki-history-list)))
        (if (or (null str) (= (length str) 0))
            default
          str)))

;;;###autoload
(defun emacs-wiki-find-file (wiki &optional command directory)
  "Open the Emacs Wiki page WIKI by name.
If COMMAND is non-nil, it is the function used to visit the file.
If DIRECTORY is non-nil, it is the directory in which the Wiki page
will be created if it does not already exist."
  (interactive
   (list
    (let ((num (prefix-numeric-value current-prefix-arg)))
       (if (< num 16)
           (let* ((file-alist (if (= num 4)
                                  (emacs-wiki-complete-alist)
                                (emacs-wiki-file-alist)))
                  (name (emacs-wiki-read-name file-alist)))
             (cons name (cdr (assoc name file-alist))))
         (let ((name (read-file-name "Open wiki file: ")))
           (cons name name))))))
  (unless (interactive-p)
    (setq wiki (cons wiki
                     (cdr (assoc wiki (emacs-wiki-file-alist))))))
  ;; At this point, `wiki' is (GIVEN-PAGE FOUND-FILE).
  (if (cdr wiki)
      (let ((buffer (funcall (or command 'find-file) (cdr wiki))))
        (if (= (prefix-numeric-value current-prefix-arg) 16)
            (with-current-buffer buffer
              (set (make-variable-buffer-local 'emacs-wiki-directories)
                   (cons (file-name-directory (cdr wiki))
                         emacs-wiki-directories))
              (set (make-variable-buffer-local 'emacs-wiki-file-alist) nil)))
        buffer)
    (let* ((dirname (or directory
                        (emacs-wiki-maybe t)
                        (car emacs-wiki-directories)))
           (filename (expand-file-name (car wiki) dirname)))
      (unless (file-exists-p dirname)
        (make-directory dirname t))
      (funcall (or command 'find-file) filename))))

;;; Navigate/visit links or URLs.  Use TAB, S-TAB and RET (or mouse-2).

(defun emacs-wiki-next-reference ()
  "Move forward to next Wiki link or URL, cycling if necessary."
  (interactive)
  (let ((case-fold-search nil)
        (cycled 0) pos)
    (save-excursion
      (if (emacs-wiki-link-at-point)
          (goto-char (match-end 0)))
      (while (< cycled 2)
        (if (re-search-forward emacs-wiki-url-or-name-regexp nil t)
            (when (get-text-property (match-beginning 0)
                                     'keymap)
              (setq pos (match-beginning 0)
                    cycled 2))
          (goto-char (point-min))
          (setq cycled (1+ cycled)))))
    (if pos
        (goto-char pos))))

(defun emacs-wiki-previous-reference ()
  "Move backward to the next Wiki link or URL, cycling if necessary.
This function is not entirely accurate, but it's close enough."
  (interactive)
  (let ((case-fold-search nil)
        (cycled 0) pos)
    (save-excursion
      (while (< cycled 2)
        (if (re-search-backward emacs-wiki-url-or-name-regexp nil t)
            (when (get-text-property (match-beginning 0)
                                     'keymap)
              (setq pos (point)
                    cycled 2))
          (goto-char (point-max))
          (setq cycled (1+ cycled)))))
    (if pos
        (goto-char pos))))

(defun emacs-wiki-browse-url (url &optional other-window)
  "Handle URL with the function specified in `emacs-wiki-url-protocols'.
If OTHER-WINDOW is non-nil, open in a different window."
  (interactive (list (read-string "URL: ")
                     current-prefix-arg))
  (when other-window
    (switch-to-buffer-other-window (current-buffer)))
  (when (string-match emacs-wiki-url-regexp url)
    (let ((entry (assoc (match-string 1 url) emacs-wiki-url-protocols)))
      (when entry
        (funcall (cadr entry) url)))))

(defun emacs-wiki-visit-link (link-name &optional refresh-buffer other-window)
  "Visit the URL or link named by LINK-NAME.
REFRESH-BUFFER is an optional buffer to refresh on saving the visited page.
This makes the bad link face in the linking buffer go away."
  (let ((link (emacs-wiki-wiki-link-target link-name))
        newbuf)
    (if (emacs-wiki-wiki-url-p link)
        (emacs-wiki-browse-url link other-window)
      ;; The name list is current since the last time the buffer was
      ;; highlighted
      (let* ((base (emacs-wiki-wiki-base link-name))
             (file (emacs-wiki-page-file base))
             (tag  (and (not (emacs-wiki-wiki-url-p link))
                        (emacs-wiki-wiki-tag link)))
             (find-file-function (if other-window
                                     'find-file-other-window
                                   'find-file)))
        (setq newbuf
              (if (null file)
                  (funcall find-file-function
                           (expand-file-name
                            base
                            (file-name-directory (buffer-file-name))))
                (funcall find-file-function file)))
        (when tag
          (goto-char (point-min))
          (unless (re-search-forward (concat "^\\.?#" tag) nil t)
            (when (string-match
                   (concat emacs-wiki-bare-digits-anchor-prefix "\\(.+\\)")
                   tag)
              (re-search-forward (concat "^\\.?#" (match-string 1 tag)
                                         "\\>")))))
        (when refresh-buffer
          (with-current-buffer newbuf
            (add-hook 'after-save-hook
                      'emacs-wiki-refresh-buffers-once t t)))))))

(defun emacs-wiki-refresh-buffers-once ()
  "Refresh all the buffers once from after-save-hook.
Call it first to add it to the hook, then the hook will remove
the function."
  (remove-hook 'after-save-hook 'emacs-wiki-refresh-buffers-once t)
  (emacs-wiki-refresh-buffers))

(defun emacs-wiki-refresh-buffers (&rest args)
  "Rebuild file alist and refresh current project.
Call after creating a page."
  (interactive)
  (emacs-wiki-refresh-file-alist)
  (let ((my-project emacs-wiki-current-project))
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (equal emacs-wiki-current-project my-project)
                (emacs-wiki-highlight-buffer))))
          (buffer-list))))

(unless (fboundp 'line-end-position)
  (defsubst line-end-position (&optional N)
    (save-excursion (end-of-line N) (point))))

(unless (fboundp 'line-beginning-position)
  (defsubst line-beginning-position (&optional N)
    (save-excursion (beginning-of-line N) (point))))

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(defun emacs-wiki-link-at-point (&optional pos)
  "Return non-nil if a URL or Wiki link name is at point."
  (if (or (null pos)
          (and (char-after pos)
               (not (eq (char-syntax (char-after pos)) ? ))))
      (let ((case-fold-search nil)
            (here (or pos (point))))
        (save-excursion
          (goto-char here)
          (skip-chars-backward "^'\"<>{}( [\t\n")
          (or (and (search-backward "[[" (line-beginning-position) t)
                   (looking-at emacs-wiki-name-regexp)
                   (<= here (match-end 0)))
              (and (goto-char here)
                   (skip-chars-backward "^'\"<>{}( [\t\n")
                   (looking-at emacs-wiki-url-or-name-regexp)))))))

(defun emacs-wiki-follow-name-at-point (&optional other-window)
  "Visit the link at point, or insert a newline if none."
  (interactive "P")
  ;; if we're visiting a bad link, pass the current buffer to the visiting
  ;; function so that it can be refreshed on saving the new page
  (let (buf)
    (when (eq (get-text-property (point) 'face)
              'emacs-wiki-bad-link-face)
      (setq buf (current-buffer)))
    (if (emacs-wiki-link-at-point)
        (emacs-wiki-visit-link (match-string 0) buf other-window)
      (error "There is no valid link at point"))))

(defun emacs-wiki-follow-name-at-point-other-window ()
  "Visit the link at point in other window."
  (interactive)
  (emacs-wiki-follow-name-at-point t))

(defun emacs-wiki-follow-name-at-mouse (event &optional other-window)
  "Visit the link at point, or yank text if none."
  (interactive "eN")
  (let (path)
    (save-excursion
      (cond ((featurep 'xemacs)    ; XEmacs
             (set-buffer (window-buffer (event-window event)))
             (and (event-point event) (goto-char (event-point event))))
            ((fboundp 'posn-window)     ; Emacs
             (set-buffer (window-buffer (posn-window (event-start event))))
             (goto-char (posn-point (event-start event)))))
      (when (emacs-wiki-link-at-point)
        (setq path (match-string 0))))
    (when path
      (emacs-wiki-visit-link path nil other-window))))

(defun emacs-wiki-follow-name-at-mouse-other-window (event)
  "Visit the link at point"
  (interactive "e")
  ;; throw away the old window position, since other-window will
  ;; change it anyway
  (select-window (car (car (cdr event))))
  (emacs-wiki-follow-name-at-mouse event t))

(defun emacs-wiki-rename-link-file (link-name new-name)
  (when (emacs-wiki-wiki-url-p link-name)
    (error "Can't rename a URL"))
  (let* ((base (emacs-wiki-wiki-base link-name))
         (file (emacs-wiki-page-file base)))
    (if (null file)
        (rename-file base new-name)
      (rename-file file new-name))))

(defun emacs-wiki-rename-link (old-link new-name)
  ;; strangely, save-excursion does not work here
  (let ((old-point (point)))
    (if (eq (aref old-link 0) ?\[)
        ;; previously an extended link - preserve it by replacing just
        ;; the name
        (replace-match new-name t t nil 3)
      ;; previously a normal link - so we replace the entire thing
      (replace-match
       (if (save-match-data
             (let ((case-fold-search nil))
               (string-match emacs-wiki-url-or-name-regexp new-name)))
           ;; normal
           new-name
         ;; it'll need [[ ]] guards
         (concat "[[" new-name "]]")) t t))
    (goto-char old-point)))

(defun emacs-wiki-rename-link-at-point ()
  "Rename the link under point, and the location it points to. This does not
  work with URLs, and will preserve a description in an extended link."
  (interactive "*")
  (let (new-name old-name)
    (if (emacs-wiki-link-at-point)
        (progn
          (setq old-name (emacs-wiki-wiki-base (match-string 0)))
          ;; emacs21 leaves the local keymap on this string, so we must strip
          ;; properties so the user can hit return to exit minibuf
          (set-text-properties 0 (length old-name) nil old-name)
          (setq new-name
                (read-from-minibuffer "Rename file to: " old-name))
          (when (string= old-name new-name)
            (error "Nothing to do"))

          (emacs-wiki-rename-link-file old-name new-name)
          (emacs-wiki-rename-link (match-string 0) new-name)
          ;; make the new link appear in the correct face
          ;; FIXME: could munge the alist instead of forcing all
          ;; mtimes to be rechecked
          (emacs-wiki-refresh-buffers))
      (error "There is no valid link at point"))))

(defun emacs-wiki-delete-link (link-name)
  "Delete the file which link-name corresponds to"
  (when (emacs-wiki-wiki-url-p link-name)
    (error "Can't rename a URL"))
  (let* ((base (emacs-wiki-wiki-base link-name))
         (file (emacs-wiki-page-file base)))
    (if (null file)
        (delete-file base)
      (delete-file file))))

(defun emacs-wiki-delete-link-at-point ()
  "Delete the link under point, and the location it points to. This does not
  work with URLs"
  (interactive "*")
  (let (name)
    (if (emacs-wiki-link-at-point)
        (progn
          (setq name (match-string 0))
          (when (yes-or-no-p (concat "Delete "
                                     name "? You can not undo this. "))
            (emacs-wiki-delete-link name)
            (replace-match "" nil t)))
      (error "There is no valid link at point"))))

;;; Find text in Wiki pages, or pages referring to the current page

(defvar emacs-wiki-search-history nil)

(defun emacs-wiki-grep (string &optional grep-command-no-shadow)
  "Grep for STRING in the Wiki directories. GREP-COMMAND if passed will
  supplant emacs-wiki-grep-command."
  ;; careful - grep-command leaks into compile, so we call it -no-shadow instead
  (require 'compile)
  (let ((str (or grep-command-no-shadow emacs-wiki-grep-command))
        (dirs (mapconcat (lambda (dir)
                           (shell-quote-argument (expand-file-name dir)))
                         emacs-wiki-directories " ")))
    (while (string-match "%W" str)
      (setq str (replace-match string t t str)))
    (while (string-match "%D" str)
      (setq str (replace-match dirs t t str)))
    (compile-internal str "No more search hits" "search"
                      nil grep-regexp-alist)))

(defun emacs-wiki-search (text)
  "Search for the given TEXT string in the Wiki directories."
  (interactive
   (list (let ((str (concat emacs-wiki-grep-command)) pos)
           (when (string-match "%W" str)
             (setq pos (match-beginning 0))
             (unless (featurep 'xemacs)
               (setq pos (1+ pos)))
             (setq str (replace-match "" t t str)))
           (read-from-minibuffer "Search command: "
                                 (cons str pos)
                                 nil nil 'emacs-wiki-search-history))))
  (emacs-wiki-grep nil text))

(defun emacs-wiki-backlink ()
  "Grep for the current pagename in all the Wiki directories."
  (interactive)
  (emacs-wiki-grep (emacs-wiki-page-name)))

;;; Generate an index of all known Wiki pages

(defvar emacs-wiki-index-title-threshold nil
  "*If nil, filenames are always used in the index.
This is faster, but less informative. If a positive integer,
only that many bytes will be scanned for a #title directive.
Else, the entire wiki file is scanned for a #title.")

(defun emacs-wiki-get-title-fast (filename)
  "Return the title in FILENAME.
Scan only the first `emacs-wiki-index-title-threshold' bytes."
  (with-temp-buffer
    (insert-file-contents
     filename
     nil
     0
     (and (integerp emacs-wiki-index-title-threshold)
          (> emacs-wiki-index-title-threshold 0)
          emacs-wiki-index-title-threshold))
    (goto-char (point-max))
    (when (re-search-backward "^#title\\s-+\\(.+\\)$" nil t)
      (match-string-no-properties 1))))

(defun emacs-wiki-file-alist-with-titles (&optional verbose)
  "Return a list of pages with titles and filenames.
If VERBOSE is non-nil, print status messages."
  (when verbose
    (message "Scanning Emacs-wiki pages for titles..."))
  (let ((len (length (emacs-wiki-file-alist))))
    (mapcar
     (lambda (item)
       (when verbose
         (if (= (% len 20) 0)
             (message "%d pages to go." len))
         (setq len (1- len)))
       (list
        (or
         (emacs-wiki-get-title-fast (cdr item))
         (emacs-wiki-prettify-title (car item))); title
        (car item) ; page
        (cdr item))) ; filename
     (emacs-wiki-file-alist))))

(defun emacs-wiki-index-files-list (&optional verbose)
  "Return a list of files to index.
If VERBOSE is non-nil, print status messages."
  (sort
   (copy-alist
    (if emacs-wiki-index-title-threshold
        (emacs-wiki-file-alist-with-titles verbose)
      (emacs-wiki-file-alist)))
   (function
    (lambda (l r)
      (string-lessp (car l) (car r))))))

(defsubst emacs-wiki-index-file-title (item) (car item))
(defsubst emacs-wiki-index-file-page (item)
  (if emacs-wiki-index-title-threshold
      (car (cdr item))
    (car item)))

(defun emacs-wiki-generate-index (&optional as-list exclude-private verbose)
  "Generate an index of all Wiki pages.
If AS-LIST is non-nil, format results as a bulleted list. If
EXCLUDE-PRIVATE is non-nil, no page satisfying
`emacs-wiki-private-p' is indexed. If VERBOSE is non-nil, status
messages are printed."
  (let ((emacs-wiki-project emacs-wiki-current-project))
    (with-current-buffer (get-buffer-create "*Wiki Index*")
      (erase-buffer)
      (when emacs-wiki-project
          (emacs-wiki-change-project emacs-wiki-project))
      (let ((files (emacs-wiki-index-files-list verbose)))
        (while files
          (unless (and exclude-private
                       (emacs-wiki-private-p
                        (emacs-wiki-index-file-page (car files))))
            (insert
             (if as-list "- " "")
             (emacs-wiki-make-link
              (emacs-wiki-index-file-page (car files))
              (emacs-wiki-index-file-title (car files)))
             "\n"))
          (setq files (cdr files)))
      (current-buffer)))))

(defun emacs-wiki-index ()
  "Display an index of all known Wiki pages."
  (interactive)
  (let ((emacs-wiki-project emacs-wiki-current-project))
    (message "Generating Wiki index...")
    (pop-to-buffer (emacs-wiki-generate-index t))
    (goto-char (point-min))
    (unless (derived-mode-p 'emacs-wiki-mode)
      (emacs-wiki-mode))
    (message "Generating Wiki index...done")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Highlighting
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-highlight nil
  "Options controlling the behaviour of Emacs Wiki highlighting.
See `emacs-wiki-highlight-buffer' for more information."
  :group 'emacs-wiki)

(defun emacs-wiki-make-faces ()
  (let ((faces '(1 2 3 4 5 6)) num newsym)
    (while faces
      (setq num (car faces))
      (setq newsym (intern (concat "emacs-wiki-header-"
                                   (int-to-string num))))
      (cond
       ((featurep 'xemacs)
        (eval `(defface ,newsym
                 '((t (:size
                       ,(nth (1- num) '("24pt" "18pt" "14pt" "12pt"))
                       :bold t)))
                 "emacs-wiki header face"
                 :group 'emacs-wiki-highlight)))
       ((< emacs-major-version 21)
        (copy-face 'default newsym))
       (t
        (eval `(defface ,newsym
                 '((t (:height ,(1+ (* 0.1 (- 5 num)))
                               :inherit variable-pitch
                               :weight bold)))
                 "emacs-wiki header face"
                 :group 'emacs-wiki-highlight))))
      (setq faces (cdr faces)))))
(emacs-wiki-make-faces)

(defface emacs-wiki-link-face
  '((((class color) (background light))
     (:foreground "blue" :underline "blue" :bold t))
    (((class color) (background dark))
     (:foreground "cyan" :underline "cyan" :bold t))
    (t (:bold t)))
  "Face for Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defface emacs-wiki-bad-link-face
  '((((class color) (background light))
     (:foreground "red" :underline "red" :bold t))
    (((class color) (background dark))
     (:foreground "coral" :underline "coral" :bold t))
    (t (:bold t)))
  "Face for bad Wiki cross-references."
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-highlight-buffer-hook nil
  "A hook run after a region is highlighted.
Each function receives three arguments: BEG END VERBOSE.
BEG and END mark the range being highlighted, and VERBOSE specifies
whether progress messages should be displayed to the user."
  :type 'hook
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-before-highlight-buffer-hook nil
  "A hook run before a region is highlighted.
Each function receives three arguments: BEG END VERBOSE.
BEG and END mark the range being highlighted, and VERBOSE specifies
whether progress messages should be displayed to the user."
  :type 'hook
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-inline-images (and (not (featurep 'xemacs))
                                         (>= emacs-major-version 21)
                                         window-system)
  "If non-nil, inline locally available images within Wiki pages."
  :type 'boolean
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-image-regexp
  "\\.\\(eps\\|gif\\|jp\\(e?g\\)\\|p\\(bm\\|ng\\)\\|tiff\\|x\\([bp]m\\)\\)\\'"
  "A link matching this regexp will be published inline as an image. Remember
that it must be matched as a link first - so use either [[CamelCaps]] or
include a leading slash - [[./text]]. An example:

  [[./wife.jpg][A picture of my wife]]

If you omit the description, the alt tag of the resulting HTML buffer will be
the name of the file."
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-file-regexp
  "[/?]\\|\\.\\(html?\\|pdf\\|el\\|zip\\|txt\\|tar\\)\\(\\.\\(gz\\|bz2\\)\\)?\\'"
  "A link matching this regexp will be regarded as a link to a file. Remember
that it must be matched as a link first - so use either [[CamelCaps]] or
include a leading slash - [[./text]]"
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-tag-regexp
  "<\\([^/ \t\n][^ \t\n</>]*\\)\\(\\s-+[^<>]+[^</>]\\)?\\(/\\)?>"
  "A regexp used to find XML-style tags within a buffer when publishing.
Group 1 should be the tag name, group 2 the properties, and group
3 the optional immediate ending slash."
  :type 'regexp
  :group 'emacs-wiki)

(defcustom emacs-wiki-inline-relative-to 'emacs-wiki-publishing-directory
  "The name of a symbol which records the location relative to where images
  should be found. The default assumes that when editing, the images can be
  found in the publishing directory. Another sensible default is
  `default-directory', which will try and find the images relative to the
  local page. You can use this to store images in wikidir/images, and
  maintain a parallel copy on the remote host."
  :type 'symbol
  :group 'emacs-wiki)

(defcustom emacs-wiki-markup-tags
  '(("example" t nil t emacs-wiki-example-tag)
    ("verbatim" t nil t emacs-wiki-verbatim-tag)
    ("nowiki" t nil t emacs-wiki-nowiki-tag)
    ("verse" t nil nil emacs-wiki-verse-tag)
    ("numbered" t nil nil emacs-wiki-numbered-tag)
    ("nop" nil nil t emacs-wiki-nop-tag)
    ("contents" nil t nil emacs-wiki-contents-tag)
    ("include" nil t nil emacs-wiki-include-tag)
    ("c-source" t t t emacs-wiki-c-source-tag)
    ("comment" t nil nil emacs-wiki-comment-tag))
  "A list of tag specifications, for specially marking up Wiki text.
XML-style tags are the best way to add custom markup to Emacs Wiki.
This is easily accomplished by customizing this list of markup tags.

For each entry, the name of the tag is given, whether it expects a
closing tag and/or an optional set of attributes, if the handler
function can also highlight the tag, and a function that performs
whatever action is desired within the delimited region.

The tags themselves are deleted during publishing, although not during
highlighting, before the function is called.  The function is called
with three arguments, the beginning and end of the region surrounded
by the tags (including the tags themselves, in the case of
highlighting).  The third argument indicates whether the purpose of
the call is to highlight the region, or mark it up for publishing.  If
properties are allowed, they are passed as a fourth argument in the
form of an alist.  The `end' argument to the function is always a
marker.

Point is always at the beginning of the region within the tags, when
the function is called.  Wherever point is when the function finishes
is where tag markup/highlighting will resume.

These tag rules are processed once at the beginning of markup, and
once at the end, to catch any tags which may have been inserted
in-between.  For highlighting, they are processed as they occur, in
the order they occur, once per text region.

Here is a summary of the default tags.  This includes the dangerous
tags listed in `emacs-wiki-dangerous-tags', which may not be used by
outsiders.

 verbatim
   Protects against highlighting and wiki interpretation, and escapes any
   characters which have special meaning to the publishing format. For HTML,
   this means characters like '<' are escaped as HTML entities.

 example
   Like verbatim, but typesets in HTML using the <pre> tag, with
   class=example, so whitespace formatting is preserved.

 nowiki
   Inhibits wiki markup, but does not do any escaping to the underlying
   publishing medium. Useful for embedding HTML, PHP, etc.

 verse
   Typesets like a normal paragraph, but without word-wrapping.
   That is, whitespace is preserved.

 redirect
   Using the \"url\" attribute, you can specify that a page should
   redirect to another page.  The remaining contents of the page will
   not be published.  The optional \"delay\" attribute specifies how
   long to wait before redirecting.

 nop
   When placed before a WikiLink, it will prevent that WikiLink from
   being treated as such.  Good for names like DocBook.

 contents
   Produces a compact table of contents for any section heading at the
   same level or lower than the next section header encountered.
   Optional \"depth\" attribute specifies how deep the table of
   contents should go.

 lisp
   Evaluate the region as a Lisp form, and displays the result.  When
   highlighting, the `display' text property is used, preserving the
   underlying text.  Turn off font-lock mode if you wish to edit it.

 command
   Pass the region to a command interpretor and insert the result,
   guarding it from any further expansion.  Optional \"file\"
   attribute specifies the shell or interpretor to use.  If none is
   given, and `emacs-wiki-command-tag-file' has not been configured,
   Eshell is used.

 python, perl
   Pass the region to the Python or Perl language interpretor, and
   insert the result.

 c-source
   Markup the region as C or C++ source code, using the c2html
   program, if available.  Optional boolean attribute \"numbered\"
   will cause source lines to be numbered.

   Note: If c2html is not available, the region will be converted to
   HTML friendly text (i.e., <> turns into &lt;&gt;), and placed in a
   <pre> block.  In this case, line numbering is not available.

 bookmarks
   Insert bookmarks at the location of the tag from the given
   bookmarks file.  Required attribute \"file\" specifies which file
   to read from, and the optional attribute \"type\" may be one of:
   adr (for Opera), lynx, msie, ns, xbel or xmlproc.  The default type
   is \"xbel\".  The optional attribute \"folder\" may be used to
   specify which folder (and its children) should be inserted.

   Note that xml-parse.el version 1.5 (available from my website) and
   the xbel-utils package (available at least to Debian users) is
   required for this feature to work."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :group 'emacs-wiki-highlight)

(defcustom emacs-wiki-dangerous-tags
  '(("redirect" t t nil emacs-wiki-redirect-tag)
    ("lisp" t nil t emacs-wiki-lisp-tag)
    ("include" nil t nil emacs-wiki-include-tag)
    ("command" t t t emacs-wiki-command-tag)
    ("python" t t t emacs-wiki-python-tag)
    ("perl" t t t emacs-wiki-perl-tag)
    ("bookmarks" nil t nil emacs-wiki-bookmarks-tag))
  "A list of tag specifications, for specially marking up Wiki text.
These tags are dangerous -- meaning represent a gaping security hole
-- and therefore are not available to outsiders who happen to edit a
Wiki page"
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :group 'emacs-wiki-highlight)

(defvar emacs-wiki-highlight-regexp nil)
(defvar emacs-wiki-highlight-vector nil)

(defun emacs-wiki-configure-highlighting (sym val)
  (setq emacs-wiki-highlight-regexp
        (concat "\\(" (mapconcat (function
                                  (lambda (rule)
                                    (if (symbolp (car rule))
                                        (symbol-value (car rule))
                                      (car rule)))) val "\\|") "\\)")
        emacs-wiki-highlight-vector (make-vector 128 nil))
  (let ((rules val))
    (while rules
      (if (eq (cadr (car rules)) t)
          (let ((i 0) (l 128))
            (while (< i l)
              (unless (aref emacs-wiki-highlight-vector i)
                (aset emacs-wiki-highlight-vector i
                      (nth 2 (car rules))))
              (setq i (1+ i))))
        (aset emacs-wiki-highlight-vector (cadr (car rules))
              (nth 2 (car rules))))
      (setq rules (cdr rules))))
  (set sym val))

(defsubst emacs-wiki-highlight-ok-context-p (beg end str)
  "Ensures whitespace or punctuation comes before the position BEG, and
  after the string STR. A search-forward is done for STR, bounding by END, and
  the position of the end of the match is returned if in the correct context."
  (save-excursion
    (let ((len (length str))
          (punctuation "\"(). "))
      (and
       (setq end (search-forward str end t))
       ;; post end, want eob or whitespace/punctuation
       (or (> (skip-syntax-forward punctuation (1+ end)) 0)
           (eq nil (char-after end)))
       (goto-char (- end len))
       ;; pre end, no whitespace
       (eq (skip-syntax-backward " " (- end len 1)) 0)
       (goto-char (+ beg len))
       ;; post beg, no whitespace
       (eq (skip-syntax-forward " " (+ beg len 1)) 0)
       (or (backward-char len) t) ;; doesn't return anything useful
       ;; pre beg, want sob or whitespace/punctuation
       (or (< (skip-syntax-backward punctuation (1- beg)) 0)
           (eq nil (char-before beg)))
       end))))

(eval-when-compile
  (defvar end))

(defun emacs-wiki-multiline-maybe (beg end &optional predicate)
  "If region between beg-end is a multi-line region, and the optional
  predicate is true, font lock the current region as multi-line. Predicate is
  called with the excursion saved."
  (when (and (or (eq (char-before end) ?\n)
                 (> (count-lines beg end) 1))
             (or (not predicate)
                 (save-excursion (funcall predicate beg end))))
    (save-excursion
      ;; mark whole lines as a multiline font-lock
      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (setq end (line-end-position))
      (add-text-properties beg end '(font-lock-multiline t))
      t)))

(defun emacs-wiki-highlight-emphasized ()
  ;; here we need to check four different points - the start and end of the
  ;; leading *s, and the start and end of the trailing *s. we allow the
  ;; outsides to be surrounded by whitespace or punctuation, but no word
  ;; characters, and the insides must not be surrounded by whitespace or
  ;; punctuation. thus the following are valid:
  ;; " *foo bar* "
  ;; "**foo**,"
  ;; and the following is invalid:
  ;; "** testing **"
  (let* ((beg (match-beginning 0))
         (e1 (match-end 0))
         (leader (- e1 beg))
         (end end)
         b2 e2 face)
    ;; if it's a header
    (unless (save-excursion
              (goto-char beg)
              (when (save-match-data (looking-at "^\\*\\{1,5\\} "))
                (add-text-properties
                 (line-beginning-position) (line-end-position)
                 (list 'face
                       (intern (concat "emacs-wiki-header-"
                                       (int-to-string (1+ leader))))))
                t))
      ;; it might be an normal, emphasised piece of text
      (when (and
             (setq e2 (emacs-wiki-highlight-ok-context-p
                       beg end (buffer-substring-no-properties beg e1)))
             (setq b2 (match-beginning 0)))
        (cond ((= leader 1) (setq face 'italic))
              ((= leader 2) (setq face 'bold))
              ((= leader 3) (setq face 'bold-italic)))
        (add-text-properties beg e1 '(invisible t intangible t))
        (add-text-properties e1 b2 (list 'face face))
        (add-text-properties b2 e2 '(invisible t intangible t)))
      (emacs-wiki-multiline-maybe
       beg end
       ;; ensures we only mark the region as multiline if it's correctly
       ;; delimited at the start
       (lambda (beg end)
         (goto-char (1+ beg))
         (eq (skip-syntax-forward " " (1+ beg)) 0)
         (or (backward-char) t)
         (or (< (skip-syntax-backward ". " (1- beg)) 0)
             (eq nil (char-before beg))))))))

(defun emacs-wiki-highlight-underlined ()
  (let ((start (- (point) 2))
        (next (1+ (match-beginning 0)))
        end)
    (when (and (not (get-text-property (point) 'invisible))
               (setq end (emacs-wiki-highlight-ok-context-p start end "_")))
      (add-text-properties start (+ start 1) '(invisible t intangible t))
      (add-text-properties (+ start 1) (- end 1) '(face underline))
      (add-text-properties (- end 1) end '(invisible t intangible t)))
    (goto-char next)))

(defun emacs-wiki-highlight-verbatim ()
  (let ((start (- (point) 2))
        end)
    (when (setq end (emacs-wiki-highlight-ok-context-p start end "="))
      (search-forward "=" end t)
      (add-text-properties start (match-end 0)
                           '(face emacs-wiki-verbatim-face)))))

(defcustom emacs-wiki-highlight-markup
  `(;; render in teletype and suppress further parsing
    ("=[^\t =]" ?= emacs-wiki-highlight-verbatim)

    ;; make emphasized text appear emphasized
    ("\\*+" ?* emacs-wiki-highlight-emphasized)

    ;; make underlined text appear underlined
    ("_[^ \t_]" ?_ emacs-wiki-highlight-underlined)

    ;; make quadruple quotes invisible
    ("''''" ?\'
     ,(function
       (lambda ()
         (add-text-properties (match-beginning 0) (match-end 0)
                              '(invisible t intangible t)))))

    ("^#title" ?\# emacs-wiki-highlight-title)

    (emacs-wiki-url-or-name-regexp t emacs-wiki-highlight-link)

    ;; highlight any markup tags encountered
    (emacs-wiki-tag-regexp ?\< emacs-wiki-highlight-custom-tags))
  "Expressions to highlight an Emacs Wiki buffer.
These are arranged in a rather special fashion, so as to be as quick as
possible.

Each element of the list is itself a list, of the form:

  (LOCATE-REGEXP TEST-CHAR MATCH-FUNCTION)

LOCATE-REGEXP is a partial regexp, and should be the smallest possible
regexp to differentiate this rule from other rules.  It may also be a
symbol containing such a regexp.  The buffer region is scanned only
once, and LOCATE-REGEXP indicates where the scanner should stop to
look for highlighting possibilities.

TEST-CHAR is a char or t.  The character should match the beginning
text matched by LOCATE-REGEXP.  These chars are used to build a vector
for fast MATCH-FUNCTION calling.

MATCH-FUNCTION is the function called when a region has been
identified.  It is responsible for adding the appropriate text
properties to change the appearance of the buffer.

This markup is used to modify the appearance of the original text to
make it look more like the published HTML would look (like making some
markup text invisible, inlining images, etc).

font-lock is used to apply the markup rules, so that they can happen
on a deferred basis.  They are not always accurate, but you can use
\\[font-lock-fontifty-block] near the point of error to force
fontification in that area.

Lastly, none of the regexp should contain grouping elements that will
affect the match data results."
  :type '(repeat
          (list :tag "Highlight rule"
                (choice (regexp :tag "Locate regexp")
                        (symbol :tag "Regexp symbol"))
                (choice (character :tag "Confirm character")
                        (const :tag "Default rule" t))
                function))
  :set 'emacs-wiki-configure-highlighting
  :group 'emacs-wiki-highlight)

(defun emacs-wiki-use-font-lock ()
  (set (make-local-variable 'font-lock-multiline) 'undecided)
  (set (make-local-variable 'font-lock-defaults)
       `(nil t nil nil 'beginning-of-line
         (font-lock-fontify-buffer-function . emacs-wiki-highlight-buffer)
         (font-lock-fontify-region-function . emacs-wiki-highlight-region)
         (font-lock-unfontify-region-function
          . emacs-wiki-unhighlight-region)))
  (set (make-local-variable 'font-lock-fontify-buffer-function)
       'emacs-wiki-highlight-buffer)
  (set (make-local-variable 'font-lock-fontify-region-function)
       'emacs-wiki-highlight-region)
  (set (make-local-variable 'font-lock-unfontify-region-function)
       'emacs-wiki-unhighlight-region)
  (font-lock-mode t))

(defun emacs-wiki-mode-flyspell-verify ()
  "Return t if the word at point should be spell checked."
  (let* ((word-pos (1- (point)))
         (props (text-properties-at word-pos)))
    (not (or (bobp)
             (memq 'display props)
             (if (and font-lock-mode (cadr (memq 'fontified props)))
                 (memq (cadr (memq 'face props))
                       '(emacs-wiki-link-face emacs-wiki-bad-link-face))
               (emacs-wiki-link-at-point word-pos))))))

(put 'emacs-wiki-mode 'flyspell-mode-predicate
     'emacs-wiki-mode-flyspell-verify)

(defun emacs-wiki-eval-lisp (form)
  "Evaluate the given form and return the result as a string."
  (require 'pp)
  (save-match-data
    (condition-case err
        (let ((object (eval (read form))))
          (cond
           ((stringp object) object)
           ((and (listp object)
                 (not (eq object nil)))
            (let ((string (pp-to-string object)))
              (substring string 0 (1- (length string)))))
           ((numberp object)
            (number-to-string object))
           ((eq object nil) "")
           (t (pp-to-string object))))
      (error
       (if (fboundp 'display-warning)
           (display-warning 'emacs-wiki
                            (format "%s/%s: Error evaluating %s: %s"
                                    emacs-wiki-current-project
                                    (emacs-wiki-page-name)
                                    form
                                    err)
                            :warning)
         (message "%s/%s: Error evaluating %s: %s"
                  emacs-wiki-current-project
                  (emacs-wiki-page-name)
                  form
                  err))
       "<!--INVALID LISP CODE-->"))))

(defun emacs-wiki-highlight-buffer ()
  "Re-highlight the entire Wiki buffer."
  (interactive)
  (emacs-wiki-highlight-region (point-min) (point-max) t))

(defun emacs-wiki-highlight-region (beg end &optional verbose)
  "Apply highlighting according to `emacs-wiki-highlight-markup'.
Note that this function should NOT change the buffer, nor should any
of the functions listed in `emacs-wiki-highlight-markup'."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (save-excursion
          (save-restriction
            (widen)
            ;; check to see if we should expand the beg/end area for
            ;; proper multiline matches
            (when (and font-lock-multiline
                       (> beg (point-min))
                       (get-text-property (1- beg) 'font-lock-multiline))
              ;; We are just after or in a multiline match.
              (setq beg (or (previous-single-property-change
                             beg 'font-lock-multiline)
                            (point-min)))
              (goto-char beg)
              (setq beg (line-beginning-position)))
            (when font-lock-multiline
              (setq end (or (text-property-any end (point-max)
                                               'font-lock-multiline nil)
                            (point-max))))
            (goto-char end)
            (setq end (line-beginning-position 2))
            ;; Undo any fontification in the area.
            (font-lock-unfontify-region beg end)
            (run-hook-with-args 'emacs-wiki-before-highlight-buffer-hook
                                beg end verbose)
            ;; And apply fontification based on `emacs-wiki-highlight-markup'
            (let ((len (float (- end beg)))
                  (case-fold-search nil))
              (goto-char beg)
              (while
                  (and (< (point) end)
                       (re-search-forward emacs-wiki-highlight-regexp end t))
                (if verbose
                    (message "Highlighting buffer...%d%%"
                             (* (/ (float (- (point) beg)) len) 100)))
                (funcall (aref emacs-wiki-highlight-vector
                               (char-after (match-beginning 0)))))
              (run-hook-with-args 'emacs-wiki-highlight-buffer-hook
                                  beg end verbose)
              (if verbose (message "Highlighting buffer...done")))))
      (set-buffer-modified-p modified-p))))

(defun emacs-wiki-unhighlight-region (begin end &optional verbose)
  "Remove all visual highlights in the buffer (except font-lock)."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t)
        (modified-p (buffer-modified-p))
        deactivate-mark)
    (unwind-protect
        (remove-text-properties
         begin end '(face nil font-lock-multiline nil
                          invisible nil intangible nil display nil
                          mouse-face nil keymap nil help-echo nil))
      (set-buffer-modified-p modified-p))))

(defvar emacs-wiki-keymap-property
  (if (or (featurep 'xemacs)
          (>= emacs-major-version 21))
      'keymap
    'local-map))

(defsubst emacs-wiki-link-properties (help-str &optional face point)
  (append (if face
              (list 'face face 'rear-nonsticky t
                    emacs-wiki-keymap-property emacs-wiki-local-map)
            (list 'invisible t 'intangible t 'rear-nonsticky t
                  emacs-wiki-keymap-property emacs-wiki-local-map))
          (list 'mouse-face 'highlight
                'help-echo help-str
                emacs-wiki-keymap-property emacs-wiki-local-map)))

(defun emacs-wiki-link-face (link-name)
  "Return the type of LINK-NAME as a face symbol - either a normal link, or a
bad-link face"
  (let ((base (emacs-wiki-wiki-base link-name)))
    (if (or (not base)
	    (emacs-wiki-page-file base)
	    (save-match-data
	      (string-match "\\(/\\|\\`[a-z]\\{3,6\\}:\\)" base)))
	'emacs-wiki-link-face
      'emacs-wiki-bad-link-face)))

(defun emacs-wiki-highlight-link ()
  (let ((start (match-beginning 0)))
    (cond
     ((and emacs-wiki-inline-images
           (match-string 6)
           (not (save-match-data (emacs-wiki-wiki-url-p (match-string 6))))
           (save-match-data (string-match emacs-wiki-image-regexp (match-string 6))))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               (match-string 6) (match-string 6)))
     ((and emacs-wiki-inline-images
           (match-string 4)
           (not (save-match-data (emacs-wiki-wiki-url-p (match-string 4))))
           (save-match-data (string-match emacs-wiki-image-regexp (match-string 4))))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               (match-string 4) (match-string 6)))
     ((and emacs-wiki-inline-images
           (save-match-data (string-match emacs-wiki-image-regexp (match-string 0))))
      (emacs-wiki-inline-image (match-beginning 0) (match-end 0)
                               (match-string 0)))
     ((match-string-no-properties 4)
      (let* ((link (match-string-no-properties 4))
             (props (emacs-wiki-link-properties
                     link
                     (emacs-wiki-link-face (match-string 4))
                         start))
                 (invis-props (append props
                                      (emacs-wiki-link-properties
                                       link
                                       nil
                                       start))))
            (if (match-string 6)
                (progn
                  ;; we put the normal face properties on the invisible
                  ;; portion too, since emacs sometimes will position
                  ;; the cursor on an intangible character
                  (add-text-properties (match-beginning 0)
                                       (match-beginning 6)
                                       invis-props)
                  (let ((text (match-string 6)))
                    (add-text-properties (match-beginning 6) (match-end 6)
                                         props)
                    (let ((escaped (save-match-data
                                     (emacs-wiki-link-unescape
                                      (match-string 6) t))))
                      (unless (equal escaped (match-string 6))
                        (add-text-properties (match-beginning 6) (match-end 6)
                                             (list 'display
                                                   (emacs-wiki-link-unescape
                                                    (match-string 6) t)
                                                   'intangible t)))))
                  (add-text-properties (match-end 6) (match-end 0)
                                       invis-props))
              (add-text-properties (match-beginning 0)
                                   (match-beginning 4) invis-props)
              (add-text-properties (match-beginning 4) (match-end 0) props)
              (add-text-properties (match-end 4) (match-end 0) invis-props))))
     (t
      (goto-char (match-end 0))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       (emacs-wiki-link-properties
        (match-string-no-properties 0)
        (emacs-wiki-link-face (match-string 0))
        start))
      (goto-char (match-end 0))))))

(defun emacs-wiki-inline-image (beg end url &optional desc)
  "Inline locally available images."
  (let ((filename
         (cond
          ((string-match "\\`file:\\(.+\\)" url)
           (match-string 1 url))
          ((string-match "/" url)
           (expand-file-name url (symbol-value
                                  emacs-wiki-inline-relative-to))))))
    (if (and filename (file-readable-p filename))
        (add-text-properties beg end (list 'display (create-image filename)
                                           'keymap emacs-wiki-local-map
                                           'help-echo (or desc url))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Wiki Publishing (to HTML by default)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-publish nil
  "Options controlling the behaviour of Emacs Wiki publishing.
See `emacs-wiki-publish' for more information."
  :group 'emacs-wiki)

(defcustom emacs-wiki-maintainer (concat "mailto:webmaster@" (system-name))
  "URL where the maintainer can be reached."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-home-page emacs-wiki-default-page
  "Title of the Wiki Home page."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-index-page "WikiIndex"
  "Title of the Wiki Index page."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-downcase-title-words
  '("the" "and" "at" "on" "of" "for" "in" "an" "a")
  "Strings that should be downcased in a Wiki page title."
  :type '(repeat string)
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-use-mode-flags (not emacs-wiki-under-windows-p)
  "If non-nil, use file mode flags to determine page permissions.
Otherwise the regexps in `emacs-wiki-private-pages' and
`emacs-wiki-editable-pages' are used."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-relative-links t
  "If non-nil, use relative interwiki links on shared servers.
If two projects share the same host part in their respective
`emacs-wiki-project-server-prefix', then use relative anchor
targets when publishing interwiki links between them."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-private-pages nil
  "A list of regexps to exclude from public view.
This variable only applies if `emacs-wiki-use-mode-flags' is nil."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-editable-pages nil
  "A list of regexps of pages that may be edited via HTTP.
This variable only applies if `emacs-wiki-use-mode-flags' is nil."
  :type '(choice (const nil) (repeat regexp))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-directory "~/WebWiki"
  "Directory where all wikis are published to."
  :type 'directory
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-prefix ""
  "This prefix will be prepended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-file-suffix ".html"
  "This suffix will be appended to all wiki names when publishing."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-before-markup-hook nil
  "A hook run in the buffer where markup is done, before it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-after-markup-hook nil
  "A hook run in the buffer where markup is done, after it is done."
  :type 'hook
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publish-function 'emacs-wiki-publish-current
  "The function used to publish a Wiki page."
  :type 'function
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-http-equiv "Content-Type"
  "The http-equiv attribute used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-content-type "text/html"
  "The content type used for the HTML <meta> tag."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-meta-content-coding
  (if (featurep 'mule)
      'detect
    "iso-8859-1")
  "If set to the symbol 'detect, use `emacs-wiki-coding-map' to try
  and determine the HTML charset from emacs's coding. If set to a string, this
  string will be used to force a particular charset"
  :type '(choice string symbol)
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-charset-default "utf-8"
  "The default HTML meta charset to use if no translation is found in
  `emacs-wiki-coding-map'"
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-coding-default 'utf-8
  "The default emacs coding  use if no special characters are found"
  :type 'symbol
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-coding-map
  '((iso-2022-jp "iso-2022-jp")
    (utf-8 "utf-8")
    (japanese-iso-8bit "euc-jp")
    (chinese-big5 "big5"))
  "An alist mapping emacs coding systems to appropriate HTML charsets.
  Use the base name of the coding system (ie, without the -unix)"
  :type '(alist :key-type coding-system :value-type (group string))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-redirect-delay 1
  "The number of seconds to delay before doing a page redirect."
  :type 'integer
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-current-page-title nil
  "Current page title, used instead of buffer name if non-nil.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defvar emacs-wiki-current-page-date nil
  "Current page date stamp, used instead of file modtime if non-nil.
This is usually set by code called by `emacs-wiki-publishing-markup'.
It should never be changed globally.")

(defcustom emacs-wiki-anchor-on-word nil
  "When true, anchors surround the closest word. This allows you
to select them in a browser (ie, for pasting), but has the
side-effect of marking up headers in multiple colours if your
header style is different to your link style."
  :type 'boolean
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-header
  "<?xml version=\"1.0\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"xhtml11.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
  <head>
    <title><lisp>(emacs-wiki-page-title)</lisp></title>
    <meta name=\"generator\" content=\"emacs-wiki.el\" />
    <meta http-equiv=\"<lisp>emacs-wiki-meta-http-equiv</lisp>\"
          content=\"<lisp>emacs-wiki-meta-content</lisp>\" />
    <link rev=\"made\" href=\"<lisp>emacs-wiki-maintainer</lisp>\" />
    <link rel=\"home\" href=\"<lisp>(emacs-wiki-published-name
                                     emacs-wiki-home-page)</lisp>\" />
    <link rel=\"index\" href=\"<lisp>(emacs-wiki-published-name
                                      emacs-wiki-index-page)</lisp>\" />
    <lisp>emacs-wiki-style-sheet</lisp>
  </head>
  <body>
    <h1 id=\"top\"><lisp>(emacs-wiki-page-title)</lisp></h1>
    <!-- Page published by Emacs Wiki begins here -->\n"
  "Text to prepend to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-footer
  "
    <!-- Page published by Emacs Wiki ends here -->
    <div class=\"navfoot\">
      <hr />
      <table width=\"100%\" border=\"0\" summary=\"Footer navigation\">
	<col width=\"33%\" /><col width=\"34%\" /><col width=\"33%\" />
        <tr>
          <td align=\"left\">
            <lisp>
              (if buffer-file-name
                  (concat
                   \"<span class=\\\"footdate\\\">Updated: \"
                   (format-time-string emacs-wiki-footer-date-format
                    (nth 5 (file-attributes buffer-file-name)))
                   (and emacs-wiki-serving-p
                        (emacs-wiki-editable-p (emacs-wiki-page-name))
                        (concat
                         \" / \"
                         (emacs-wiki-link-href
                          (concat \"editwiki?\" (emacs-wiki-page-name))
                          \"Edit\")))
                   \"</span>\"))
            </lisp>
          </td>
          <td align=\"center\">
            <span class=\"foothome\">
              <lisp>
                (concat
                 (and (emacs-wiki-page-file emacs-wiki-home-page t)
                      (not (emacs-wiki-private-p emacs-wiki-home-page))
                      (concat
                       (emacs-wiki-link-href emacs-wiki-home-page \"Home\")
                       \" / \"))
                 (emacs-wiki-link-href emacs-wiki-index-page \"Index\")
                 (and (emacs-wiki-page-file \"ChangeLog\" t)
                      (not (emacs-wiki-private-p \"ChangeLog\"))
                      (concat
                       \" / \"
                       (emacs-wiki-link-href \"ChangeLog\" \"Changes\"))))
              </lisp>
            </span>
          </td>
          <td align=\"right\">
            <lisp>
              (if emacs-wiki-serving-p
                  (concat
                   \"<span class=\\\"footfeed\\\">\"
                   (emacs-wiki-link-href \"searchwiki?get\" \"Search\")
                   (and buffer-file-name
                        (concat
                         \" / \"
                         (emacs-wiki-link-href
                          (concat \"searchwiki?q=\" (emacs-wiki-page-name))
                          \"Referrers\")))
                   \"</span>\"))
            </lisp>
          </td>
        </tr>
      </table>
    </div>
  </body>
</html>\n"
  "Text to append to a wiki being published.
This text may contain <lisp> markup tags."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-footer-date-format "%Y-%m-%d"
  "Format of current date for `emacs-wiki-publishing-footer'.
This string must be a valid argument to `format-time-string'."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-style-sheet
  "<style type=\"text/css\">
a.nonexistent {
  font-weight: bold;
  background-color: #F8F8F8; color: #FF2222;
}

a.nonexistent:visited {
  background-color: #F8F8F8; color: #FF2222;
}

body {
  background: white; color: black;
  margin-left: 5%; margin-right: 5%;
  margin-top: 3%;
}

em { font-style: italic; }
strong { font-weight: bold; }

ul { list-style-type: disc }

dl.contents { margin-top: 0; }
dt.contents { margin-bottom: 0; }

p.verse {
  white-space: pre;
  margin-left: 5%;
}

pre {
  white-space: pre;
  font-family: monospace;
  margin-left: 5%;
}
</style>"
  "The style sheet used for each wiki page.
This can either be an inline stylesheet, using <style></style> tags,
or an external stylesheet reference using a <link> tag.

Here is an example of using a <link> tag:

  <link rel=\"stylesheet\" type=\"text/css\" href=\"emacs-wiki.css\" />"
  :type 'string
  :group 'emacs-wiki-publish)

(defvar emacs-wiki-publishing-p nil
  "Set to t while Wiki pages are being published.
This can be used by <lisp> tags to know when HTML is being generated.")

(defcustom emacs-wiki-block-groups-regexp
  "\\(h[1-9r]\\|[oud]l\\|table\\|center\\|blockquote\\|pre\\)[^>]*"
  "This regexp identifies HTML tag which defines their own blocks.
That is, they do not need to be surrounded by <p>."
  :type 'regexp
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-table-attributes
  "class=\"ewiki-table\" border=\"2\" cellpadding=\"5\""
  "The attribute to be used with HTML <table> tags.
Note that since emacs-wiki support direct insertion of HTML tags, you
can easily create any kind of table you want, as long as every line
begins at column 0 (to prevent it from being blockquote'd).  To make
really ANYTHING you want, use this idiom:

  <verbatim>
  <table>
    [... contents of my table, in raw HTML ...]
  </verbatim></table>

It may look strange to have the tags out of sequence, but remember
that verbatim is processed long before table is even seen."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-report-threshhold 100000
  "If a Wiki file is this size or larger, report publishing progress."
  :type 'integer
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-publishing-markup
  (list
   ["&\\([-A-Za-z_#0-9]+\\);" 0 emacs-wiki-markup-entity]

   ;; change the displayed title or the stylesheet for a given page
   ["^#\\(title\\|date\\|style\\)\\s-+\\(.+\\)\n+" 0
    emacs-wiki-markup-initial-directives]

   ;; process any markup tags
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags]

   ;; emphasized or literal text
   ["\\(^\\|[-[ \t\n<('`\"]\\)\\(=[^= \t\n]\\|_[^_ \t\n]\\|\\*+[^* \t\n]\\)"
    2 emacs-wiki-markup-word]

   ;; headings, outline-mode style
   ["^\\(\\*+\\)\\s-+" 0 emacs-wiki-markup-heading]

   ;; define anchor points
   ["^#\\(\\S-+\\)\\s-*" 0 emacs-wiki-markup-anchor]

   ;; horizontal rule, or section separator
   ["^----+" 0 "<hr />"]

   ;; footnotes section is separated by a horizontal rule in HTML
   ["^\\(\\* \\)?Footnotes:?\\s-*" 0 "</p><hr />\n<p>\n"]
   ;; footnote definition/reference (def if at beginning of line)
   ["\\[\\([1-9][0-9]*\\)\\]" 0 emacs-wiki-markup-footnote]

   ;; don't require newlines between numbered and unnumbered lists.
   ;; This must come before paragraphs are calculated, so that any
   ;; extra newlines added will be condensed.
   ["^\\s-*\\(-\\s-\\|[0-9]+\\.\\s-\\)" 1 "\n\\1"]

   ;; the beginning of the buffer begins the first paragraph
   ["\\`\n*" 0 "<p>\n"]
   ;; plain paragraph separator
   ["\n\\([ \t]*\n\\)+" 0 "\n\n</p>\n\n<p>\n"]

   ;; simple table markup. use | to separate cells, || to separate header
   ;; elements, and ||| for footer elements
   ["^\\s-*\\(\\([^|\n]+\\(|+\\)\\s-*\\)+\\)\\([^|\n]+\\)?$"
    1 emacs-wiki-markup-table]

   ;; unnumbered List items begin with a -.  numbered list items
   ;; begin with number and a period.  definition lists have a
   ;; leading term separated from the body with ::.  centered
   ;; paragraphs begin with at least six columns of whitespace; any
   ;; other whitespace at the beginning indicates a blockquote.  The
   ;; reason all of these rules are handled here, is so that
   ;; blockquote detection doesn't interfere with indented list
   ;; members.
   ["^\\(\\s-*\\(-\\|[0-9]+\\.\\|\\(.+\\)[ \t]+::\n?\\)\\)?\\([ \t]+\\)" 4
    emacs-wiki-markup-list-or-paragraph]

   ;; "verse" text is indicated the same way as a quoted e-mail
   ;; response: "> text", where text may contain initial whitespace
   ;; (see below).
   ["<p>\\s-+> \\(\\([^\n]\n?\\)+\\)\\(\\s-*</p>\\)?" 0
    emacs-wiki-markup-verse]

   ;; join together the parts of a list
   ["</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" 0 ""]

   ;; join together the parts of a table
   (vector
    (concat "</tbody>\\s-*"
            "</table>\\s-*" "\\(</p>\\s-*<p>\\s-*\\)?" "<table[^>]*>\\s-*"
            "<tbody>\\s-*") 0 "")
   ["</table>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<table[^>]*>\\s-*" 0 ""]

   ;; fixup paragraph delimiters
   (vector
    (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp ">\\)") 0 "\\1")
   (vector (concat "\\(</?" emacs-wiki-block-groups-regexp
                   ">\\)\\s-*\\(</p>\\)") 3 "\\1")

   ;; terminate open paragraph at the end of the buffer
   ["<p>\\s-*\\'" 0 ""]
   ;; make sure to close any open text (paragraphs)
   ["\\([^> \t\n]\\)\\s-*\\'" 0 "\\1\n</p>"]

   ;; replace WikiLinks in the buffer (links to other pages)
   ;; <nop> before a WikiName guards it from being replaced
   ;; '''' can be used to add suffixes, such as WikiName''''s
   [emacs-wiki-url-or-name-regexp 0 emacs-wiki-markup-link]
   ["''''" 0 ""]

   ;; bare email addresses
   (vector
    (concat
     "\\([^-+:.@/a-zA-Z0-9]\\)"
     "\\([-+a-zA-Z0-9._]+@\\([-a-zA-Z0-9_]+\\.\\)+[a-zA-Z0-9]+\\)"
     "\\([^-\"a-zA-Z0-9]\\)")
    0
    "\\1<a href=\"mailto:\\2\">\\2</a>\\4")

   ;; replace quotes, since most browsers don't understand `` and ''
   ["\\(``\\|''\\)" 0 "\""]

   ;; insert the default publishing header
   (function
    (lambda ()
      (insert emacs-wiki-publishing-header)))

   ;; insert the default publishing footer
   (function
    (lambda ()
      (goto-char (point-max))
      (insert emacs-wiki-publishing-footer)))

   ;; process any remaining markup tags
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags])
  "List of markup rules to apply when publishing a Wiki page.
Each member of the list is either a function, or a vector of the form:

  [REGEXP/SYMBOL TEXT-BEGIN-GROUP REPLACEMENT-TEXT/FUNCTION/SYMBOL].

REGEXP is a regular expression, or symbol whose value is a regular
expression, which is searched for using `re-search-forward'.
TEXT-BEGIN-GROUP is the matching group within that regexp which
denotes the beginning of the actual text to be marked up.
REPLACEMENT-TEXT is a string that will be passed to `replace-match'.
If it is not a string, but a function, it will be called to determine
what the replacement text should be (it must return a string).  If it
is a symbol, the value of that symbol should be a string.

The replacements are done in order, one rule at a time.  Writing the
regular expressions can be a tricky business.  Note that case is never
ignored.  `case-fold-search' is always be bound to nil while
processing the markup rules.

Here is a description of the default markup rules:

Headings

 * First level
 ** Second level
 *** Third level

 Note that the first level is actually indicated using H2, so that
 it doesn't appear at the same level as the page heading (which
 conceptually titles the section of that Wiki page).

Horizontal rules

----

Emphasis

 *emphasis*
 **strong emphasis**
 ***very strong emphasis***
 _underlined text_
 =verbatim=

 <verbatim>This tag should be used for larger blocks of
 text</verbatim>.

Footnotes

  A reference[1], which is just a number in square brackets,
  constitutes a footnote reference.

  Footnotes:

  [1]  Footnotes are defined by the same number in brackets
       occurring at the beginning of a line.  Use footnote-mode's C-c
       ! a command, to very easily insert footnotes while typing.  Use
       C-x C-x to return to the point of insertion.

Paragraphs

  One or more blank lines separates paragraphs.

Centered paragraphs and quotations

  A line that begins with six or more columns of whitespace (made up
  of tabs or spaces) indicates a centered paragraph.  I assume this
  because it's expected you will use M-s to center the line, which
  usually adds a lot of whitespace before it.

  If a line begins with some whitespace, but less than six columns, it
  indicates a quoted paragraph.

Poetic verse

  Poetry requires that whitespace be preserved, without resorting to
  the monospace typical of <pre>.  For this, the following special
  markup exists, which is reminiscent of e-mail quotations:

    > A line of Emacs verse;
    > forgive its being so terse.

  You can also use the <verse> tag, if you prefer:

    <verse>
    A line of Emacs verse;
    forgive its being so terse.
    </verse>

Literal paragraphs

  Use the HTML tags <pre></pre> to insert a paragraph and preserve
  whitespace.  If you're inserting a block of code, you will almost
  always want to use <verbatim></verbatim> *within* the <pre> tags.
  The shorcut for doing this is to use the <example> tag:

    <example>
    Some literal text or code here.
    </example>

Lists

  - bullet list

  1. Enumerated list

  Term :: A definition list

  Blank lines between list elements are optional, but required between
  members of a definition list.

Tables

  There are two forms of table markup supported.  If Takaaki Ota's
  table.el package is available, then simply create your tables using
  his package, and they will be rendered into the appropriate HTML.
  You need to (require 'emacs-wiki-table) for this functionality.

  If table.el is not available, then only very simple table markup is
  supported.  The attributes of the table are kept in
  `emacs-wiki-table-attributes'.  The syntax is:

    Double bars || Separate header fields
    Single bars | Separate body fields
    Here are more | body fields
    Triple bars ||| Separate footer fields

  Other paragraph markup applies to both styles, meaning that if six
  or more columns of whitespace precedes the first line of the table,
  it will be centered, and if any whitespace at all precedes first
  line, it will occur in a blockquote.

Anchors and tagged links

  #example If you begin a line with \"#anchor\" -- where anchor
  can be any word that doesn't contain whitespace -- it defines an
  anchor at that point into the document.  This anchor text is not
  displayed.

  You can reference an anchored point in another page (or even in the
  current page) using WikiName#anchor.  The #anchor will never be
  displayed in HTML, whether at the point of definition or reference,
  but it will cause browsers to jump to that point in the document.

Redirecting to another page or URL

  Sometimes you may wish to redirect someone to another page.  To do
  this, put:

    <redirect url=\"http://somewhereelse.com\"/>

  at the top of the page.  If the <redirect> tag specifies content,
  this will be used as the redirection message, rather than the
  default.

  The numbers of seconds to delay is defined by
  `emacs-wiki-redirect-delay', which defaults to 2 seconds.  The page
  shown will also contain a link to click on, for browsing which do
  not support automatic refreshing.

URLs

  A regular URL is given as a link.  If it's an image URL, it will
  be inlined using an IMG tag.

Embedded lisp

  <lisp>(concat \"This form gets\" \"inserted\")</lisp>

Special handling of WikiNames

  If you need to add a plural at the end of a WikiName, separate it
  with four single quotes (WikiName''''s) or make it an explicit
  link ([[WikiName]]s).

  To prevent a link name (of any type) from being treated as such,
  surround it with =equals= (to display it in monotype), or prefix it
  with the tag <nop> to escape it from WikiName markup.

Special Wiki links

  Besides the normal WikiName type links, emacs-wiki also supports
  extended links:

    [[link text][optional link description]]

  An extended link is always a link, no matter how it looks.  This
  means you can use any file in your `emacs-wiki-directories' as a
  Wiki file.  If you provide an optional description, that's what will
  be shown instead of the link text.  This is very useful for
  providing textual description of URLs.

  See the documentation to emacs-wiki-image-regexp for how to inline
  files and images.

InterWiki names

  There are times when you will want to constantly reference pages on
  another website.  Rather than repeating the URL ad nauseum, you can
  define an InterWiki name.  This is a set of WikiNames to URL
  correlations, that support textual substitution using #anchor names
  (which are appended to the URL).  For example, MeatballWiki is
  defined in the variable `emacs-wiki-interwiki-names'.  It means you
  can reference the page \"MeatBall\" on MeatballWiki using this
  syntax:

    MeatballWiki#MeatBall

  In the resulting HTML, the link is simply shown as
  \"MeatballWiki:MeatBall\"."
  :type '(repeat
          (choice
           (vector :tag "Markup rule"
                   (choice regexp symbol)
                   integer
                   (choice string function symbol))
           function))
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-changelog-markup
  (list
   ;; process any custom markup tags
   [emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags]

   ["&" 0 "&amp;"]
   ["<" 0 "&lt;"]
   [">" 0 "&gt;"]

   ["^\\(\\S-+\\)\\s-+\\(.+\\)" 0 emacs-wiki-markup-changelog-section]

   ;; emphasized or literal text
   ["\\(^\\|[-[ \t\n<('`\"]\\)\\(=[^= \t\n]\\|_[^_ \t\n]\\|\\*+[^* \t\n]\\)"
    2 emacs-wiki-markup-word]

   ;; headings, outline-mode style
   ["^\\*\\s-+\\(.+\\)$" 0 "<h2>\\1</h2>"]

   ;; escape the 'file' entries, incase they are extended wiki links
   ["^[ \t]+\\* \\([^:(]+\\)\\([ \t]+(\\|:\\)" 0 emacs-wiki-changelog-escape-files]

   ;; don't require newlines between unnumbered lists.
   ["^\\s-*\\(\\*\\)" 1 "\n\\1"]

   ;; the beginning of the buffer begins the first paragraph
   ["\\`\n*" 0 "<p>\n"]
   ;; plain paragraph separator
   ["\n\\([ \t]*\n\\)+" 0 "\n\n</p>\n\n<p>\n"]

   ;; unnumbered List items begin with a -.  numbered list items
   ;; begin with number and a period.  definition lists have a
   ;; leading term separated from the body with ::.  centered
   ;; paragraphs begin with at least six columns of whitespace; any
   ;; other whitespace at the beginning indicates a blockquote.  The
   ;; reason all of these rules are handled here, is so that
   ;; blockquote detection doesn't interfere with indented list
   ;; members.
   ["^\\(\\s-*\\(\\*\\)\\)?\\([ \t]+\\)\\(\\([^\n]\n?\\)+\\)" 3
    "<ul>\n<li>\n\\4</li></ul>\n"]

   ;; join together the parts of a list
   ["</\\([oud]l\\)>\\s-*\\(</p>\\s-*<p>\\s-*\\)?<\\1>\\s-*" 0 ""]

   ;; fixup paragraph delimiters
   (vector
    (concat "<p>\\s-*\\(</?" emacs-wiki-block-groups-regexp ">\\)") 0 "\\1")
   (vector (concat "\\(</?" emacs-wiki-block-groups-regexp
                   ">\\)\\s-*\\(</p>\\)") 3 "\\1")

   ;; terminate open paragraph at the end of the buffer
   ["<p>\\s-*\\'" 0 ""]
   ;; make sure to close any open text (paragraphs)
   ["\\([^> \t\n]\\)\\s-*\\'" 0 "\\1\n</p>"]

   ;; bare email addresses
   (vector
    (concat
     "\\([^-+:.@/a-zA-Z0-9]\\)"
     "\\([-+a-zA-Z0-9._]+@\\([-a-zA-Z0-9_]+\\.\\)+[a-zA-Z0-9]+\\)"
     "\\([^-\"a-zA-Z0-9]\\)")
    0
    "\\1<a href=\"mailto:\\2\">\\2</a>\\4")

   ;; replace WikiLinks in the buffer (links to other pages)
   [emacs-wiki-url-or-name-regexp 0 emacs-wiki-markup-link]
   ["''''" 0 ""]

   ;; insert the default publishing header
   (function
    (lambda ()
      (if (file-readable-p emacs-wiki-publishing-header)
	  (insert-file-contents emacs-wiki-publishing-header)
	(insert emacs-wiki-publishing-header))))

   ;; insert the default publishing footer
   (function
    (lambda ()
      (goto-char (point-max))
      (if (file-readable-p emacs-wiki-publishing-footer)
	  (insert-file-contents emacs-wiki-publishing-footer)
	(insert emacs-wiki-publishing-footer)))))
  "List of markup rules for publishing ChangeLog files.
These are used when the wiki page's name is ChangeLog."
  :type '(repeat
          (choice
           (vector :tag "Markup rule"
                   (choice regexp symbol)
                   integer
                   (choice string function symbol))
           function))
  :group 'emacs-wiki-publish)

(defun emacs-wiki-transform-content-type (content-type)
  "Using `emacs-wiki-coding-map', try and resolve an emacs coding
  system to an associated HTML coding system. If no match is found,
  `emacs-wiki-charset-default' is used instead."
  (let ((match (assoc (coding-system-base content-type)
                      emacs-wiki-coding-map)))
    (if match
        (cadr match)
      emacs-wiki-charset-default)))

(defun emacs-wiki-private-p (name)
  "Return non-nil if NAME is a private page, and shouldn't be published."
  (if name
      (if emacs-wiki-use-mode-flags
          (let* ((page-file (emacs-wiki-page-file name))
                 (filename (and page-file (file-truename page-file))))
            (if filename
                (or (eq ?- (aref (nth 8 (file-attributes
                                         (file-name-directory filename))) 7))
                    (eq ?- (aref (nth 8 (file-attributes filename)) 7)))))
        (let ((private-pages emacs-wiki-private-pages) private)
          (while private-pages
            (if (string-match (car private-pages) name)
                (setq private t private-pages nil)
              (setq private-pages (cdr private-pages))))
          private))))

(defun emacs-wiki-editable-p (name)
  "Return non-nil if NAME is a page that may be publically edited."
  nil) ;; Unless you load emacs-wiki-httpd.el, no pages can be edited.

(defun emacs-wiki-visit-published-file (&optional arg)
  "Visit the current wiki page's published result."
  (interactive "P")
  (if arg
      (find-file-other-window (emacs-wiki-published-file))
    (run-hook-with-args-until-success
     'emacs-wiki-browse-url-functions
     (concat "file:" (emacs-wiki-published-file)))))

(defun emacs-wiki-dired-publish ()
  "Publish all marked files in a dired buffer."
  (interactive)
  (emacs-wiki-publish-files (dired-get-marked-files) t))

(defun emacs-wiki-prettify-title (title)
  "Prettify the given TITLE."
  (save-match-data
    (let ((case-fold-search nil))
      (while (string-match "\\([A-Za-z]\\)\\([A-Z0-9]\\)" title)
        (setq title (replace-match "\\1 \\2" t nil title)))
      (let* ((words (split-string title))
             (w (cdr words)))
        (while w
          (if (member (downcase (car w))
                      emacs-wiki-downcase-title-words)
              (setcar w (downcase (car w))))
          (setq w (cdr w)))
        (mapconcat 'identity words " ")))))

(defun emacs-wiki-publish-index ()
  "Publish an index of the Wiki pages.
This function can be added to `emacs-wiki-after-wiki-publish-hook'."
  (interactive)
  (with-current-buffer (emacs-wiki-generate-index t t t)
    (message "Marking up index...")
    (emacs-wiki-replace-markup emacs-wiki-index-page)
    (let ((backup-inhibited t))
      (write-file (emacs-wiki-published-file emacs-wiki-index-page)))
    (kill-buffer (current-buffer))))

(defun emacs-wiki-publish (&optional arg)
  "Publish all wikis that need publishing.
If the published wiki already exists, it is only overwritten if the
wiki is newer than the published copy.  When given the optional
argument ARG, all wikis are rewritten, no matter how recent they are.
The index file is rewritten no matter what."
  (interactive "P")
  ;; prompt to save any emacs-wiki buffers
  (save-some-buffers nil (lambda ()
                           (derived-mode-p 'emacs-wiki-mode)))

  (if (emacs-wiki-publish-files
       (let* ((names (emacs-wiki-file-alist))
              (files (list t))
              (lfiles files))
         (while names
           (setcdr lfiles (cons (cdar names) nil))
           (setq lfiles (cdr lfiles)
                 names (cdr names)))
         (cdr files)) arg)
      (progn
        (run-hooks 'emacs-wiki-after-wiki-publish-hook)
        (message "All Wiki pages%s have been published."
                 (if emacs-wiki-current-project
                     (concat " for project " emacs-wiki-current-project)
                   "")))
    (message "No Wiki pages%s need publishing at this time."
             (if emacs-wiki-current-project
                 (concat " in project " emacs-wiki-current-project)
               ""))))

(defun emacs-wiki-publish-this-page ()
  "Force publication of the current page."
  (interactive)
  (emacs-wiki-publish-files (list buffer-file-name) t))

(defvar emacs-wiki-after-file-publish-hook nil "Hook run after every file is published.")
(defvar emacs-wiki-after-wiki-publish-hook '(emacs-wiki-publish-index) "Hook run after the files have been published through `emacs-wiki-publish'.")


(defun emacs-wiki-write-buffer (output-path)
  (let ((backup-inhibited t)
	(buffer-file-coding-system
	 (when (boundp 'buffer-file-coding-system)
	   buffer-file-coding-system))
	(find-file-literally t))
    (when (eq buffer-file-coding-system 'undecided-unix)
      ;; make it agree with the default charset
      (setq buffer-file-coding-system
	    emacs-wiki-coding-default))
    (write-file output-path)))

(defun emacs-wiki-publish-current (file output-path)
  (with-temp-buffer
    (insert-file-contents file t)
    (cd (file-name-directory file))
    (emacs-wiki-maybe)
    (emacs-wiki-replace-markup)
    (emacs-wiki-write-buffer output-path)))

(defun emacs-wiki-publish-files (files force)
  "Publish all files in list FILES.
If the argument FORCE is nil, each file is only published if it is
newer than the published version.  If the argument FORCE is non-nil,
the file is published no matter what."
  (let (published-some file page published)
    (while files
      (setq file (car files)
            files (cdr files)
            page (emacs-wiki-page-name file)
            published (emacs-wiki-published-file page))
      ;; ensure the publishing location is available
      (let ((publishing-directory (file-name-directory published)))
        (unless (file-exists-p publishing-directory)
          (message "Creating publishing subdirectory %s" publishing-directory)
          (make-directory publishing-directory 'parents)))
      (when (and (not (emacs-wiki-private-p page))
               (or force (file-newer-than-file-p file published)))
        (funcall emacs-wiki-publish-function file published)
        (run-hook-with-args 'emacs-wiki-after-file-publish-hook file)
        (setq published-some t)))
    published-some))

(defun emacs-wiki-escape-html-specials (&optional end)
  (while (and (or (not end) (< (point) end))
              (re-search-forward "[<>&\"]" end t))
    (cond
     ((eq (char-before) ?\")
      (delete-char -1)
      (insert "&quot;"))
     ((eq (char-before) ?\<)
      (delete-char -1)
      (insert "&lt;"))
     ((eq (char-before) ?\>)
      (delete-char -1)
      (insert "&gt;"))
     ((eq (char-before) ?\&)
      (delete-char -1)
      (insert "&amp;")))))

;; Copied from w3m-url-encode-string (w3m.el)
(defun emacs-wiki-escape-url (str &optional coding)
  "Hexify dangerous characters in STR.
If CODING is used, use that coding system."
  (save-match-data
    (apply (function concat)
           (mapcar
            (lambda (ch)
              (cond
               ((eq ch ?\n)		; newline
                "%0D%0A")
               ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
                (char-to-string ch))	; printable
               ((char-equal ch ?\x20)	; space
                "+")
               (t
                (format "%%%02x" ch))))	; escape
            ;; Coerce a string to a list of chars.
            (string-to-list str)))))

;; we currently only do this on links. this means a stray '&' in an
;; emacs-wiki document risks being misinterpreted when being published, but
;; this is the price we pay to be able to inline HTML content without special
;; tags.
(defun emacs-wiki-escape-html-string (str)
  "Convert to character entities any non alphanumeric characters outside of a
  few punctuation symbols, that risk being misinterpreted if not escaped"
  (when str
    (let (pos code len)
      (save-match-data
        (while (setq pos (string-match "[^-[:alnum:]/:._=@\\?~#]" str pos))
          (setq code (int-to-string (aref str pos))
                len (length code)
                str (replace-match (concat "&#" code ";") nil nil str)
                pos (+ 3 len pos)))
        str))))

(eval-when-compile
  (defvar emacs-wiki-meta-content))

(defun emacs-wiki-replace-markup (&optional title)
  "Replace markup according to `emacs-wiki-publishing-markup'."
  (let* ((emacs-wiki-meta-http-equiv emacs-wiki-meta-http-equiv)
         (emacs-wiki-current-page-title title)
         emacs-wiki-current-page-date
         (emacs-wiki-publishing-p t)
         (case-fold-search nil)
         (inhibit-read-only t)
         (rules (if (string= (emacs-wiki-page-name) "ChangeLog")
                    emacs-wiki-changelog-markup
                  emacs-wiki-publishing-markup))
         (limit (* (length rules) (point-max)))
         (verbose (and emacs-wiki-report-threshhold
                       (> (point-max) emacs-wiki-report-threshhold)))
         (base 0)
         (emacs-wiki-meta-content
          (concat emacs-wiki-meta-content-type "; charset="
                  (if (stringp emacs-wiki-meta-content-coding)
                      emacs-wiki-meta-content-coding
                    (emacs-wiki-transform-content-type
                     (or buffer-file-coding-system
                         emacs-wiki-coding-default))))))
    (run-hooks 'emacs-wiki-before-markup-hook)
    (while rules
      (goto-char (point-min))
      (if (functionp (car rules))
          (funcall (car rules))
        (let ((regexp (aref (car rules) 0))
              (group (aref (car rules) 1))
              (replacement (aref (car rules) 2))
              start last-pos pos)
          (if (symbolp regexp)
              (setq regexp (symbol-value regexp)))
          (if verbose
              (message "Publishing %s...%d%%"
                       (emacs-wiki-page-name)
                       (* (/ (float (+ (point) base)) limit) 100)))
          (while (and regexp (setq pos (re-search-forward regexp nil t)))
            (if verbose
                (message "Publishing %s...%d%%"
                         (emacs-wiki-page-name)
                         (* (/ (float (+ (point) base)) limit) 100)))
            (unless (get-text-property (match-beginning group) 'read-only)
              (let ((text (cond
                           ((functionp replacement)
                            (funcall replacement))
                           ((symbolp replacement)
                            (symbol-value replacement))
                           (t replacement))))
                (when text
                  (condition-case nil
                      (replace-match text t)
                    (error
                     (replace-match "[FIXME: invalid characters]" t))))))
                (if (and last-pos (= pos last-pos))
                    (if (eobp)
                        (setq regexp nil)
                      (forward-char 1)))
                (setq last-pos pos))))
          (setq rules (cdr rules)
                base (+ base (point-max))))
        (run-hooks 'emacs-wiki-after-markup-hook)
        (if verbose
            (message "Publishing %s...done" (emacs-wiki-page-name)))))

(defun emacs-wiki-custom-tags (&optional highlight-p)
  (let ((tag-info (or (assoc (match-string 1) emacs-wiki-markup-tags)
                      (assoc (match-string 1) emacs-wiki-dangerous-tags))))
    (when (and tag-info (or (not highlight-p)
                            (nth 3 tag-info)))
      (let ((closed-tag (match-string 3))
            (start (match-beginning 0))
            (beg (point)) end attrs)
        (when (nth 2 tag-info)
          (let ((attrstr (match-string 2)))
            (while (and attrstr
                        (string-match
                         "\\([^ \t\n=]+\\)\\(=\"\\([^\"]+\\)\"\\)?" attrstr))
              (let ((attr (cons (downcase
                                 (match-string-no-properties 1 attrstr))
                                (match-string-no-properties 3 attrstr))))
                (setq attrstr (replace-match "" t t attrstr))
                (if attrs
                    (nconc attrs (list attr))
                  (setq attrs (list attr)))))))
        (if (and (cadr tag-info) (not closed-tag))
            (if (search-forward (concat "</" (car tag-info) ">") nil t)
                (unless highlight-p
                  (delete-region (match-beginning 0) (point)))
              (setq tag-info nil)))
        (when tag-info
          (setq end (point-marker))
          (unless highlight-p
            (delete-region start beg))
          (goto-char (if highlight-p beg start))
          (let ((args (list start end)))
            (if (nth 2 tag-info)
                (nconc args (list attrs)))
            (if (nth 3 tag-info)
                (nconc args (list highlight-p)))
            (apply (nth 4 tag-info) args))))))
  nil)

(defun emacs-wiki-markup-initial-directives ()
  (cond
   ((string= (match-string 1) "title")
    (set (make-local-variable 'emacs-wiki-current-page-title)
	 (match-string 2)))
   ((string= (match-string 1) "date")
    (set (make-local-variable 'emacs-wiki-current-page-date)
	 (match-string 2)))
   ((string= (match-string 1) "style")
    (set (make-local-variable 'emacs-wiki-style-sheet)
          (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\""
                  (match-string 2) "\" />"))))
  "")

(defalias 'emacs-wiki-markup-custom-tags 'emacs-wiki-custom-tags)

(defun emacs-wiki-highlight-title ()
  (add-text-properties (+ 7 (match-beginning 0))
                       (line-end-position)
                       '(face emacs-wiki-header-1)))

(defun emacs-wiki-highlight-custom-tags ()
  ;; Remove the match-data related to the url-or-name-regexp, which is
  ;; part of emacs-wiki-highlight-regexp.  All in the name of speed.
  (let ((match-data (match-data)))
    (setcdr (cdr match-data)
            (nthcdr (* 2 (+ 2 emacs-wiki-url-or-name-regexp-group-count))
                    match-data))
    (set-match-data match-data)
    (emacs-wiki-custom-tags t)))

;; This should be the very last tag highlighted.
(defun emacs-wiki-example-tag (beg end highlight-p)
  (if highlight-p
      (progn
        (remove-text-properties
         beg end '(face nil font-lock-multiline nil
                        invisible nil intangible nil display nil
                        mouse-face nil keymap nil help-echo nil))
        (goto-char end))
    (insert "<pre class=\"example\">")
    (emacs-wiki-escape-html-specials end)
    (when (< (point) end)
      (goto-char end))
    (insert "</pre>")
    (add-text-properties beg (point) '(rear-nonsticky (read-only)
                                                      read-only t))))

(defface emacs-wiki-verbatim-face
  '((t (:foreground "gray")))
  "Face for verbatim text.")

(defun emacs-wiki-verbatim-tag (beg end highlight-p)
  (if highlight-p
      (progn
        (emacs-wiki-multiline-maybe beg end)
        (add-text-properties beg end '(face emacs-wiki-verbatim-face))
        (goto-char end))
    (emacs-wiki-escape-html-specials end)
    (add-text-properties beg end '(rear-nonsticky (read-only)
                                                  read-only t))))

(defun emacs-wiki-nowiki-tag (beg end highlight-p)
  (if highlight-p
      (goto-char end)
    (add-text-properties
     beg end '(read-nonsticky (read-only) read-only t))))

(defun emacs-wiki-verse-tag (beg end)
  (save-excursion
    (while (< (point) end)
      (unless (eq (char-after) ?\n)
        (insert "> "))
      (forward-line))))

(defvar emacs-wiki-numbered-counter 1)
(make-variable-buffer-local 'emacs-wiki-numbered-counter)

(defun emacs-wiki-numbered-tag (beg end)
  (save-excursion
    (goto-char beg)
    (setq end (copy-marker (1- end)))
    (insert "<table cellspacing=\"8\">")
    (insert (format "<tr><td valign=\"top\"><strong>%d</strong></td>
<td><p><a id=\"%d\"/>" emacs-wiki-numbered-counter
                         emacs-wiki-numbered-counter))
    (setq emacs-wiki-numbered-counter
          (1+ emacs-wiki-numbered-counter))
    (while (and (< (point) end)
                (re-search-forward "^$" end t))
      (replace-match (format "</p>
</td>
</tr><tr><td valign=\"top\"><strong>%d</strong></td><td>
<p id=\"%d\">" emacs-wiki-numbered-counter
                     emacs-wiki-numbered-counter))
      (setq emacs-wiki-numbered-counter
            (1+ emacs-wiki-numbered-counter)))
    (goto-char end)
    (insert "</p>
</td></tr></table>")))

(defun emacs-wiki-redirect-tag (beg end attrs)
  (let ((link (cdr (assoc "url" attrs))))
    (when link
      (setq emacs-wiki-meta-http-equiv "Refresh"
            emacs-wiki-meta-content
            (concat (or (cdr (assoc "delay" attrs))
                        (int-to-string emacs-wiki-redirect-delay))
                    ";\nURL=" (emacs-wiki-link-url link)))
      (if (= beg end)
          (insert "You should momentarily be redirected to [[" link "]].")
        (goto-char end))
      (delete-region (point) (point-max)))))

(defun emacs-wiki-nop-tag (beg end highlight-p)
  (when (<= (- end beg) 5)
    (if highlight-p
        (add-text-properties beg end '(invisible t intangible t)))
    (when (looking-at emacs-wiki-name-regexp)
      (goto-char (match-end 0))
      (unless highlight-p
        (add-text-properties beg (match-end 0)
                             '(rear-nonsticky (read-only) read-only t))))))

(defun emacs-wiki-insert-anchor (anchor)
  "Insert an anchor, either around the word at point, or within a tag."
  (skip-chars-forward " \t\n")
  (when (string-match "^[0-9]+" anchor)
    (setq anchor (concat emacs-wiki-bare-digits-anchor-prefix anchor)))
  (if (looking-at "<\\([^ />]+\\)>")
      (let ((tag (match-string 1)))
        (goto-char (match-end 0))
        (insert "<a id=\"" anchor "\">")
        (when emacs-wiki-anchor-on-word
          (or (and (search-forward (format "</%s>" tag)
                                   (line-end-position) t)
                   (goto-char (match-beginning 0)))
              (forward-word 1)))
        (insert "</a>"))
    (insert "<a id=\"" anchor "\">")
    (when emacs-wiki-anchor-on-word
      (forward-word 1))
    (insert "</a>")))

(defun emacs-wiki-contents-tag (beg end attrs)
  (let ((max-depth (let ((depth (cdr (assoc "depth" attrs))))
                     (or (and depth (string-to-int depth)) 2)))
        (index 1)
        base contents l)
    (save-excursion
      (catch 'done
        (while (re-search-forward "^\\(\\*+\\)\\s-+\\(.+\\)" nil t)
          (setq l (length (match-string 1)))
          (if (null base)
              (setq base l)
            (if (< l base)
                (throw 'done t)))
          (when (<= l max-depth)
            (setq contents (cons (cons l (match-string-no-properties 2))
                                 contents))
            (goto-char (match-beginning 2))
            (emacs-wiki-insert-anchor (concat "sec" (int-to-string index)))
            (setq index (1+ index))))))
    (setq index 1 contents (reverse contents))
    (let ((depth 1) (sub-open 0) (p (point)))
      (insert "<dl class=\"contents\">\n")
      (while contents
        (insert "<dt class=\"contents\">\n")
        (insert "<a href=\"#sec" (int-to-string index) "\">"
                (cdar contents)
                "</a>\n")
        (setq index (1+ index))
        (insert "</dt>\n")
        (setq depth (caar contents)
              contents (cdr contents))
        (if contents
            (cond
             ((< (caar contents) depth)
              (let ((idx (caar contents)))
                (while (< idx depth)
                  (insert "</dl>\n</dd>\n")
                  (setq sub-open (1- sub-open)
                        idx (1+ idx)))))
             ((> (caar contents) depth) ; can't jump more than one ahead
              (insert "<dd>\n<dl class=\"contents\">\n")
              (setq sub-open (1+ sub-open))))))
      (while (> sub-open 0)
        (insert "</dl>\n</dd>\n")
        (setq sub-open (1- sub-open)))
      (insert "</dl>\n")
      (put-text-property p (point) 'read-only t))))

(defun emacs-wiki-comment-tag (beg end)
  (delete-region beg end))

(defun emacs-wiki-lisp-tag (beg end highlight-p)
  (if highlight-p
      (add-text-properties
       beg end
       (list 'font-lock-multiline t
             'display (emacs-wiki-eval-lisp
                       (buffer-substring-no-properties (+ beg 6) (- end 7)))
             'intangible t))
    (save-excursion
      (insert (emacs-wiki-eval-lisp
               (prog1
                   (buffer-substring-no-properties beg end)
                 (delete-region beg end)))))))

(defcustom emacs-wiki-command-default-file nil
  "If non-nil, this default program to use with <command> tags.
If nil, Eshell is used, since it works on all platforms."
  :type '(choice file (const :tag "Use Eshell" nil))
  :group 'emacs-wiki-publish)

(defun emacs-wiki-command-tag (beg end attrs &optional highlight-p pre-tags)
  (if highlight-p
      (goto-char end)
    (while (looking-at "\\s-*$")
      (forward-line))
    (let ((interp (or (cdr (assoc "file" attrs))
                      emacs-wiki-command-default-file)))
      (if (null interp)
          (eshell-command (prog1
                              (buffer-substring-no-properties (point) end)
                            (delete-region beg end)) t)
        (let ((file (make-temp-file "ewiki")))
          (unwind-protect
              (let ((args (split-string interp)))
                (write-region (point) end file)
                (delete-region beg end)
                (if pre-tags
                    (insert "<pre>\n"))
                (apply 'call-process (car args) file t nil (cdr args))
                (while (eq (char-syntax (char-before)) ? )
                  (backward-char))
                (add-text-properties beg (point)
                                     '(rear-nonsticky (read-only)
                                                      read-only t))
                (if pre-tags
                    (insert "</pre>\n")))
            (if (file-exists-p file)
                (delete-file file))))))))

(defcustom emacs-wiki-c-to-html
  (if (or (featurep 'executable)
          (load "executable" t t))
      (concat (executable-find "c2html") " -c -s"))
  "Program to use to convert <c-source> tag text to HTML."
  :type 'string
  :group 'emacs-wiki-publish)

(defun emacs-wiki-c-source-tag (beg end attrs highlight-p)
  (if highlight-p
      (goto-char end)
    (if emacs-wiki-c-to-html
        (let ((c-to-html emacs-wiki-c-to-html))
          (if (assoc "numbered" attrs)
              (setq c-to-html (concat c-to-html " -n")))
          (emacs-wiki-command-tag beg end (list (cons "file" c-to-html))))
      (insert "<pre>")
      (emacs-wiki-escape-html-specials end)
      (goto-char end)
      (add-text-properties beg (point)
                           '(rear-nonsticky (read-only) read-only t))
      (insert "</pre>"))))

(defun emacs-wiki-python-tag (beg end attrs highlight-p)
  (emacs-wiki-command-tag
   beg end (list (cons "file" (executable-find "python"))) highlight-p t))

(defun emacs-wiki-perl-tag (beg end attrs highlight-p)
  (emacs-wiki-command-tag
   beg end (list (cons "file" (executable-find "perl"))) highlight-p t))

(defun emacs-wiki-insert-xbel-bookmarks (bmarks folder)
  "Insert a set of XBEL bookmarks as an HTML list."
  (require 'xml-parse)
  (when (fboundp 'xml-tag-name)
    (while bmarks
      (let ((bookmark (car bmarks)))
        (cond
         ((equal (xml-tag-name bookmark) "folder")
          (let ((title (cadr (xml-tag-child bookmark "title"))))
            (unless folder
              (insert "<li>" title "\n<ul>\n"))
            (emacs-wiki-insert-xbel-bookmarks (xml-tag-children bookmark)
                                              (if (equal folder title)
                                                  nil
                                                folder))
            (unless folder
              (insert "</ul>\n"))))
         ((equal (xml-tag-name bookmark) "bookmark")
          (unless folder
            (insert "<li><a href=\"" (xml-tag-attr bookmark "href") "\">"
                    (cadr (xml-tag-child bookmark "title")) "</a>\n")))))
      (setq bmarks (cdr bmarks)))))

(defcustom emacs-wiki-xbel-bin-directory "/usr/bin"
  "Directory where the xbel parsing utilities reside."
  :type 'directory
  :group 'emacs-wiki-publish)

(defun emacs-wiki-include-tag (beg end attrs)
  "Include the named file at the current location during publishing.

<include file=\"...\">

Includes the named file at the current location during
publishing. Files are marked up according to the emacs-wiki rules
except for inserting header and footer. If you want no markup to
be performed, either add <example>..</example> inside the source
file or use

<include file=\"...\" markup=\"nil\">

The markup attribute controls how this section is marked up. If
non-nil, it should be the name of a function to call after
inserting the file with the buffer narrowed to the section
inserted. Note that no further marking-up will be performed on
this region."
  (let ((filename (expand-file-name (cdr (assoc "file" attrs))))
        (markup (cdr (assoc "markup" attrs)))
        (emacs-wiki-publishing-header "")
        (emacs-wiki-publishing-footer ""))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (insert-file-contents filename)
        (if markup
            (let ((markup-function (read markup)))
              (when markup-function
                (funcall markup-function))
          (emacs-wiki-replace-markup))
        (add-text-properties (point-min)
                             (point-max)
                             '(rear-nonsticky (read-only) read-only t)))))))

(defun emacs-wiki-bookmarks-tag (beg end attrs)
  (require 'xml-parse)
  (let ((filename (expand-file-name (cdr (assoc "file" attrs))))
        (type (cdr (assoc "type" attrs)))
        (folder (cdr (assoc "folder" attrs)))
        (this-buffer (current-buffer))
        buffer)
    (when filename
      (cond
       (type
        (setq buffer (get-buffer-create " *xbel_parse*"))
        (with-current-buffer buffer
          (erase-buffer)
          (call-process
           (format "%s/%s_parse"
                   (directory-file-name emacs-wiki-xbel-bin-directory) type)
           nil t nil filename)))
       (t
        (setq buffer (find-file-noselect filename))))
      (insert "<ul>\n")
      (emacs-wiki-insert-xbel-bookmarks
       (with-current-buffer buffer
         (goto-char (point-min))
         (when (re-search-forward "<!DOCTYPE\\s-+xbel" nil t) ; XBEL format
           (goto-char (match-beginning 0))
           ;; the `cdr' is to skip the "title" child
           (cdr (xml-tag-children (read-xml))))) folder)
      (insert "</ul>\n")
      (kill-buffer buffer)))
  (while (eq (char-syntax (char-before)) ? )
    (backward-char))
  (add-text-properties beg (point)
                       '(rear-nonsticky (read-only) read-only t)))

(defun emacs-wiki-link-url (wiki-link)
  "Resolve the given WIKI-LINK into its ultimate URL form."
  (let ((link (emacs-wiki-wiki-link-target wiki-link)))
     (save-match-data
        (if (or (emacs-wiki-wiki-url-p link)
                (string-match emacs-wiki-image-regexp link)
                (string-match emacs-wiki-file-regexp link))
            link
          (if (assoc (emacs-wiki-wiki-base link)
                     (emacs-wiki-file-alist))
              (if (string-match "#" link)
                  (concat
                   (emacs-wiki-escape-url
                    (emacs-wiki-published-name
                     (substring link 0 (match-beginning 0))
                     (emacs-wiki-page-name)))
                   "#"
                   (substring link (match-end 0)))
                (emacs-wiki-escape-url
                 (emacs-wiki-published-name link
                                            (emacs-wiki-page-name)))))))))

(defsubst emacs-wiki-link-href (url name)
  "Return an href string for URL and NAME."
  (concat "<a href=\"" (emacs-wiki-published-name url) "\">" name "</a>"))

(defun emacs-wiki-markup-link ()
  "Resolve the matched wiki-link into its ultimate <a href> form.
Images used the <img> tag."
  ;; avoid marking up urls that appear to be inside existing HTML
  (save-match-data
    (when (and (not (eq (char-after (point)) ?\"))
               (not (eq (char-after (point)) ?\>)))
      (let* (string
             (wiki-link (match-string 0))
             (url (emacs-wiki-link-url wiki-link))
             (text (emacs-wiki-wiki-visible-name wiki-link))
             (name text))
        (when (and url (string-match emacs-wiki-url-regexp url))
          (let ((protocol (assoc (match-string 1 url)
                                 emacs-wiki-url-protocols)))
            (when protocol
              (setq url
                    (if (functionp (elt protocol 2))
                        (save-match-data
                          (funcall (elt protocol 2) url))
                      (elt protocol 2))))))
        ;; Replace protocols in the name, too
        (when (and name (string-match emacs-wiki-url-regexp name))
          (let ((protocol (assoc (match-string 1 name)
                                 emacs-wiki-url-protocols)))
            (when protocol
              (setq name
                    (replace-match
                     (if (functionp (elt protocol 2))
                         (save-match-data
                           (funcall (elt protocol 2) (match-string 0 name)))
                       (elt protocol 2))
                     t t name)))))
        (when name
          (setq name
                (if (string-match emacs-wiki-image-regexp name)
                    (if (string-match "^\\([^ ]+\\)\\s-+\\(.+\\)" name)
                        ;; [[image][image2 caption]]
                        (format "<img src=\"%s\" alt=\"%s\"/>"
                                (match-string 1 name)
                                (emacs-wiki-escape-html-string (match-string 2 name)))
                      ;; [[image][image2]]
                      (concat "<img src=\"" name "\">"))
                  ;; [[image][caption]]
                  (if (and url (string-match emacs-wiki-image-regexp url))
                      (format "<img src=\"%s\" alt=\"%s\"/>"
                              url (emacs-wiki-escape-html-string name))
                    ;; [[link][caption]]
                    (emacs-wiki-escape-html-string name)))))
        (setq string
              (if (null url)
                  (if (and emacs-wiki-serving-p
                           (emacs-wiki-editable-p
                            (emacs-wiki-wiki-base wiki-link)))
                      (format
                       "<a class=\"nonexistent\" href=\"editwiki?%s\">%s</a>"
                       (emacs-wiki-wiki-base wiki-link) name)
                    (format "<a class=\"nonexistent\" href=\"%s\">%s</a>"
                            emacs-wiki-maintainer name))
                (if (string= url "")
                    name
                  (format "<a href=\"%s\">%s</a>"
                          url name))))
        (add-text-properties 0 (1- (length string))
                             '(rear-nonsticky (read-only) read-only
                                              t) string)
      string))))

(defun emacs-wiki-markup-word ()
  (let* ((beg (match-beginning 2))
         (end (1- (match-end 2)))
         (leader (buffer-substring-no-properties beg end))
         open-tag close-tag mark-read-only loc multi-line)
    (cond
     ((string= leader "_")
      (setq open-tag "<u>" close-tag "</u>"))
     ((string= leader "=")
      (setq open-tag "<code>" close-tag "</code>")
      (setq mark-read-only t))
     (t
      (setq multi-line t)
      (let ((l (length leader)))
        (cond
         ((= l 1) (setq open-tag "<em>" close-tag "</em>"))
         ((= l 2) (setq open-tag "<strong>" close-tag "</strong>"))
         ((= l 3) (setq open-tag "<strong><em>"
                        close-tag "</em></strong>"))))))
    (if (and (setq loc (search-forward leader nil t))
             (eq 0 (skip-syntax-forward "w" (1+ loc)))
             (or multi-line (= 1 (count-lines beg loc))))
        (progn
          (replace-match "")
          (insert close-tag)
          (save-excursion
            (goto-char beg)
            (delete-region beg end)
            (insert open-tag))
          (if mark-read-only
              (add-text-properties beg (point)
                                   '(rear-nonsticky (read-only) read-only
                                   t))))
      (backward-char))
    nil))

(defun emacs-wiki-markup-anchor ()
  (save-match-data
    (emacs-wiki-insert-anchor (match-string 1)))
  "")

(defcustom emacs-wiki-entity-table
  '(("#7779" . "s")
    ("#7717" . "h")
    ("#7789" . "t")
    ("#7716" . "H")
    ("#7826" . "Z"))
  "Substitutions to use for HTML entities which are not fully
supported by all browsers -- in other words, we are pre-empting the
entity mechanism and providing our own textual equivalent.  For
Unicode browsers, this is usually unnecessary."
  :type 'sexp
  :group 'emacs-wiki)

(defun emacs-wiki-markup-entity ()
  (or (cdr (assoc (match-string 1)
                  emacs-wiki-entity-table))
      (concat "&" (match-string 1) ";")))

(defsubst emacs-wiki-surround-text (beg-tag end-tag move-func)
  (insert beg-tag)
  (funcall move-func)
  (insert end-tag))                     ; returns nil for us

(defun emacs-wiki-markup-heading ()
  (let ((len (1+ (length (match-string 1)))))
    (emacs-wiki-surround-text (format "<h%d>" len) (format "</h%d>" len)
                              'end-of-line)
    ""))

(defun emacs-wiki-markup-footnote ()
  (if (/= (line-beginning-position) (match-beginning 0))
      "<sup><a id=\"fnr.\\1\" href=\"#fn.\\1\">\\1</a></sup>"
    (prog1
        "<sup>[<a id=\"fn.\\1\" href=\"#fnr.\\1\">\\1</a>]</sup>"
      (save-excursion
        (save-match-data
          (let* ((beg (goto-char (match-end 0)))
                 (end (and (search-forward "\n\n" nil t)
                           (prog1
                               (copy-marker (match-beginning 0))
                             (goto-char beg)))))
            (while (re-search-forward "^[ \t]+\\([^\n]\\)" end t)
              (replace-match "\\1" t))))))))

(defsubst emacs-wiki-forward-paragraph ()
  (and (re-search-forward "^\\s-*$" nil t)
       (match-beginning 0)))

(defun emacs-wiki-markup-list-or-paragraph ()
  "Markup a list entry or quoted paragraph.
The reason this function is so funky, is to prevent text properties
like read-only from being inadvertently deleted."
  (if (null (match-string 2))
      (let* ((ws (match-string 4))
             (tag (if (>= (string-width ws) 6)
                      "center"
                    "blockquote")))
        (unless (and (equal tag "blockquote")
                     (save-excursion
                       (forward-line)
                       (or (eolp)
                           (looking-at "\\S-"))))
          (emacs-wiki-surround-text (format "<%s>\n<p>\n%s" tag ws)
                                    (format "\n</p>\n</%s>\n" tag)
                                    'emacs-wiki-forward-paragraph)))
    (let ((str (match-string 2)))
      (cond
       ((and (eq (aref str 0) ?-))
        (delete-region (match-beginning 0) (match-end 0))
        (emacs-wiki-surround-text
         "<ul>\n<li>" "</li>\n</ul>\n"
         (function
          (lambda ()
            (and (re-search-forward "^\\s-*\\(-\\s-\\|$\\)" nil t)
                 (goto-char (match-beginning 0)))))))
       ((and (>= (aref str 0) ?0)
             (<= (aref str 0) ?9))
        (delete-region (match-beginning 0) (match-end 0))
        (emacs-wiki-surround-text
         "<ol>\n<li>" "</li>\n</ol>\n"
         (function
          (lambda ()
            (and (re-search-forward "^\\s-*\\([0-9]+\\.\\s-\\|$\\)" nil t)
                 (goto-char (match-beginning 0)))))))
       (t
        (goto-char (match-beginning 0))
        (insert "<dl>\n<dt>")
        (save-match-data
          (when (re-search-forward "[ \t\n]+::[ \t\n]+" nil t)
            (replace-match "</dt>\n<dd>\n")))
        (emacs-wiki-forward-paragraph)
        (insert "</dd>\n</dl>\n"))))))

(defun emacs-wiki-markup-table ()
  "Mark up tables normally."
  (let* ((str (save-match-data
                (emacs-wiki-replace-regexp-in-string
                 " *|+i'l *$" ""
                 (match-string 1))))
         (fields
          (append (save-match-data
                    (remove "" (split-string str "[ \t]*|+[ \t]*")))
                  (list (match-string 4))))
         (len (length (match-string 3)))
         (row (cond ((= len 1) "tbody")
                    ((= len 2) "thead")
                    ((= len 3) "tfoot")))
         (col (cond ((= len 1) "td")
                    ((= len 2) "th")
                    ((= len 3) "td"))))
    (concat "<table " emacs-wiki-table-attributes ">\n"
            "<" row ">\n" "<tr>\n<" col ">"
            (mapconcat 'identity fields (format "</%s><%s>" col col))
            "</" col ">\n" "</tr>\n" "</" row ">\n"
            "</table>\n")))

(defun emacs-wiki-markup-verse ()
  (save-match-data
    (let* ((lines (split-string (match-string 1) "\n"))
           (l lines))
      (while l
        (if (and (> (length (car l)) 2)
                 (string-match "\\`\\s-*> " (car l)))
            (setcar l (substring (car l) (match-end 0))))
        (setq l (cdr l)))
      (concat "<p class=\"verse\">"
	      (mapconcat 'identity lines "<br>\n") "</p>"))))

(defcustom emacs-wiki-pretty-changelogs nil
  "If non-nil, markup ChangeLog buffers using pretty tables.
This rule requires that a GIF file called \"onepixel.gif\" be in your
publication tree.  Here is a uuencoded version of such a file:

begin 644 onepixel.gif
M1TE&.#EA`0`!`*$``````/___________R'Y!`'__P$`+``````!``$```(\"
$3`$`.P``
`
end"
  :type 'boolean
  :group 'emacs-wiki-publish)

(defun emacs-wiki-changelog-escape-files ()
  (replace-match "[[\\1]]" t nil nil 1))

(defun emacs-wiki-markup-changelog-section ()
  (if (not emacs-wiki-pretty-changelogs)
      "* \\1 \\2"
    (let ((email (match-string 2))
          (date (match-string 1)))
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (while (eolp)
        (kill-line 1))
      (insert (format "  <table class=\"changelog table parent\"
         width=\"100%%\" border=\"0\" cellspacing=\"1\"
         cellpadding=\"2\">
    <tr>
      <td style=\"background: #000 url(onepixel.gif);\">
        <table width=\"100%%\" border=\"0\"
               cellpadding=\"5\" cellspacing=\"0\">
          <tr>
            <td class=\"changelog table author\" style=\"color: navy;
                background: #b0c4de url(onepixel.gif); text-align: left;\">
              <b>%s</b>
            </td>
            <td class=\"changelog table email\" style=\"color: #2f4f4f;
                font-size: 90%%; background: #b0c4de url(onepixel.gif);
                text-align: right; vertical-align: bottom;\">
               %s
            </td>
          </tr>
          <tr>
            <td class=\"changelog table changes\" style=\"color: #000;
                background: #fffff0 url(onepixel.gif);\" colspan=\"2\">
" email date))
      (add-text-properties (match-beginning 0) (point)
                           '(read-only t rear-nonsticky (read-only))))
    (if (re-search-forward "^[0-9]" nil t)
        (goto-char (1- (match-beginning 0)))
      (goto-char (point-max))
      (while (eq (char-before (1- (point))) ?\n)
        (delete-char -1)))
    (let ((here (1- (point))))
      (insert "
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </table>
  <br />")
      (add-text-properties here (point)
                           '(read-only t rear-nonsticky (read-only)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Support for multile Emacs Wiki projects
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup emacs-wiki-project nil
  "Options controlling multi-project behavior in Emacs-Wiki."
  :group 'emacs-wiki)

(defvar emacs-wiki-current-project nil)
(defvar emacs-wiki-predicate nil)
(defvar emacs-wiki-major-mode nil)
(defvar emacs-wiki-project-server-prefix nil)

(defcustom emacs-wiki-show-project-name-p t
  "When true, display the current project name in the mode-line"
  :group 'emacs-wiki
  :type 'boolean)

(defcustom emacs-wiki-update-project-hook
  '(emacs-wiki-update-project-interwikis)
  "A hook called whenever `emacs-wiki-projects' is modified.
By default, this hook is used to update the Interwiki table so that it
contains links to each project name."
  :type 'hook
  :group 'emacs-wiki-project)

(defun emacs-wiki-update-project-interwikis ()
  (let ((projs emacs-wiki-projects))
    (while projs
      (add-to-list
       'emacs-wiki-interwiki-names
       `(,(caar projs)
         . (lambda (tag)
             (emacs-wiki-project-interwiki-link ,(caar projs) tag))))
      (setq projs (cdr projs)))))

;; (setq emacs-wiki-projects
;;      `((\"MainWiki\" . ((emacs-wiki-directories . (\"~/wiki\"))))
;;        (\"work\" . ((fill-column . 65)
;;                 (emacs-wiki-directories . (\"~/workwiki/\"))))))

(defcustom emacs-wiki-projects nil
  "A list of project-specific Emacs-Wiki variable settings.
Each entry is a cons cell, of the form (PROJECT VARS).
Projects are useful for maintaining separate wikis that vary in
some way.

You can change between projects with
\\[emacs-wiki-change-project], by default bound to C-c C-v. When
you use \\[emacs-wiki-find-file] to find a new file, emacs-wiki
will attempt to detect which project it is part of by finding the
first project where emacs-wiki-directories contains that file.

VARS is an alist of symbol to value mappings, to be used locally in
all emacs-wiki buffers associated with that PROJECT.

You may also set the variable `emacs-wiki-predicate' in this alist,
which should be a function to determine whether or not the project
pertains to a certain buffer.  It will be called within the buffer in
question.  The default predicate checks whether the file exists within
`emacs-wiki-directories' for that project.

The variable `emacs-wiki-major-mode' can be used to determine the
major mode for a specific emacs-wiki buffer, in case you have
developed a customized major-mode derived from `emacs-wiki-mode'.

The variable `emacs-wiki-project-server-prefix' is prepended to the
Interwiki URL, whenever an Interwiki reference to another project is
made.  For example, if you had two projects, A and B, and in A you
made a reference to B by typing B#WikiPage, A needs to know what
directory or server to prepend to the WikiPage.html href.  If this
variable is not set, it is assumed that both A and B publish to the
same location.

If any variable is not customized specifically for a project, the
global value is used."
  :type '(alist
          :key-type (string :tag "Project name")
          :value-type (alist
                       :key-type (symbol :tag "Variable")
                       :value-type (sexp :tag "Value")))
  :set (function
        (lambda (sym val)
          (set sym val)
          (run-hooks 'emacs-wiki-update-project-hook)))
  :group 'emacs-wiki-project)

(defmacro with-emacs-wiki-project (project &rest body)
  "Evaluate as part of PROJECT the given BODY forms."
  `(with-temp-buffer
     (emacs-wiki-change-project ,project)
     ,@body))

(put 'with-emacs-wiki-project 'lisp-indent-function 1)

(defun emacs-wiki-change-project (project)
  "Change wiki projects.

When called interactively, load the welcome page of the selected
project in a new buffer. If no project is selected, the default
project as specified in `emacs-wiki-default-project' will be used.

Note that the project will only be changed if the welcome page
exists for the target project. This may be changed in the future
to find a non-existent file, though if this happens it is not
clear which of wiki directory should be used in the case of there
being multiple directories.

When called from a lisp program, update the current buffer's
project to PROJECT."
  (interactive (list (completing-read "Switch to project: "
                                      emacs-wiki-projects
                                      nil t nil)))
  (when (string= "" project)
    (setq project emacs-wiki-default-project))
  (let ((projsyms (cdr (assoc project emacs-wiki-projects)))
        sym)
    (while projsyms
      (setq sym (caar projsyms))
      (unless (memq sym '(emacs-wiki-predicate emacs-wiki-major-mode))
        (let ((custom-set (or (get sym 'custom-set) 'set))
              (var (if (eq (get sym 'custom-type) 'hook)
                       (make-local-hook sym)
                     (make-local-variable sym))))
          (if custom-set
              (funcall custom-set var (cdar projsyms)))))
      (setq projsyms (cdr projsyms))))

  (let ((current emacs-wiki-current-project))
    (if (and (interactive-p)
             (not (string= current project)))
        ;; when changing projects interactively, jump to the welcome
        ;; page of the new project, and don't clobber the existing
        ;; buffer
        (with-emacs-wiki-project project
          (emacs-wiki-visit-link emacs-wiki-default-page))
      ;; change to the new project, and update modeline if appropriate
      (set (make-local-variable 'emacs-wiki-current-project) project)
      (when emacs-wiki-show-project-name-p
        (setq mode-name (concat "Wiki[" project "]"))))))

(defun emacs-wiki-relative-link-maybe (dest src)
  "Return the relative link for DEST based on SRC."
  (let ((dest-host
         (and (string-match emacs-wiki-url-server-regexp dest)
              (match-string 3 dest)))
        (src-host
         (and (string-match emacs-wiki-url-server-regexp src)
              (match-string 3 src))))
    (and dest-host src-host (string= dest-host src-host)
         (file-relative-name dest src))))

(defun emacs-wiki-project-interwiki-link (project tag)
  (let ((page-publishing-directory
         (file-name-directory
          (concat emacs-wiki-project-server-prefix
                  (emacs-wiki-link-url (emacs-wiki-page-name))))))
    (with-emacs-wiki-project project
      (if emacs-wiki-publishing-p
          (let ((url (emacs-wiki-link-url (or tag emacs-wiki-home-page))))
            (cond
             ;; bad link, no prefix will be added
             ((null url) "")
             ;; try and convert to a relative link
             ((and emacs-wiki-relative-links
                   ;; without catching extended links by mistake
                   (not (string-match "\\[\\[[^][]+\\(\\]\\[[^][]+\\)?\\]\\]"
                                      url))
                   (emacs-wiki-relative-link-maybe
                    (concat emacs-wiki-project-server-prefix url)
                    page-publishing-directory)))
             ;; use the server prefix
             ((concat emacs-wiki-project-server-prefix url))))
        (or (emacs-wiki-page-file (or tag emacs-wiki-home-page))
            ;; doesn't yet exist, so we don't qualify the name, causing it
            ;; to be rendered as a bad link
            tag)))))

;; URLs

(defun emacs-wiki-resolve-url-google (url)
  "Return the correct Google search string."
  (when (string-match "^google:/?/?\\(.+\\)" url)
    (concat "http://www.google.com/search?q="
            (match-string 1 url))))

(defun emacs-wiki-browse-url-google (url)
  "If this is a Google URL, jump to it."
  (let ((google-url (emacs-wiki-resolve-url-google url)))
    (when google-url
      (browse-url google-url))))

(defun emacs-wiki-browse-url-info (url)
  "If this in an Info URL, jump to it."
  (require 'info)
  (cond
   ((string-match "^info://\\([^#]+\\)#\\(.+\\)" url)
    (Info-find-node (match-string 1 url)
                    (match-string 2 url)))
   ((string-match "^info://\\([^#]+\\)" url)
    (Info-find-node (match-string 1 url)
                    "Top"))
   ((string-match "^info://(\\([^)]+\\))\\(.+\\)" url)
    (Info-find-node (match-string 1 url) (match-string 2 url)))
   ((string-match "^info://\\(.+\\)" url)
    (Info-find-node (match-string 1 url) "Top"))))

(defun emacs-wiki-browse-url-man (url)
  "If this in a manpage URL, jump to it."
  (cond ((string-match "^man://\\(.+\\):\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url)
                               "(" (match-string 2 url) ")")))
        ((string-match "^man://\\(.+\\)" url)
         (manual-entry (concat (match-string 1 url))))))


(provide 'emacs-wiki)
;;; emacs-wiki.el ends here
