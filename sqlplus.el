;;; sqlplus.el --- User friendly interface to SQL*Plus and support for PL/SQL compilation

;; Copyright (C) 2007 Piotr Karpiuk, Scott Tiger S.A.

;; Author: Piotr Karpiuk <Piotr.Karpiuk@tiger.com.pl>
;; Maintainer: Piotr Karpiuk <Piotr.Karpiuk@tiger.com.pl>
;; Created: 25 Nov 2007
;; Version 0.8
;; Keywords: sql sqlplus oracle plsql

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Facilitates interaction with Oracle via SQL*Plus (GNU Emacs only).
;;  Moreover, this package complements plsql.el (Kahlil Hodgson) 
;;  upon convenient compilation of PL/SQL source files.
;;
;;  This package was inspired by sqlplus-mode.el (Rob Riepel, Peter
;;  D. Pezaris, Martin Schwenke), but offers more features:
;;    - tables are parsed, formatted and rendered with colors, like in
;;      many GUI programs; you can see raw SQL*Plus output also, 
;;      if you wish
;;    - table will be cutted if you try to fetch too many rows
;;      (SELECT * FROM MY_MILLION_ROWS_TABLE); current SQL*Plus command
;;      will be automatically interrupted under the hood in such cases
;;    - you can use many SQL*Plus processes simultaneously,
;;    - font locking (especially if you use Emacs>=22), with database
;;      object names highlighting,
;;    - history (log) of executed commands - see` sqlplus-history-dir`
;;      variable,
;;    - commands for fetching any database object definition
;;      (package, table/index/sequence script)
;;    - query result can be shown in HTML,
;;    - input buffer for each connection can be saved into file on
;;      disconnect and automatically restored on next connect (see
;;      'sqlplus-session-cache-dir' variable); if you place some
;;      SQL*Plus commands between '/* init */' and '/* end */'
;;      comments in saved input buffer, they will be automatically
;;      executed on every connect
;;    - if you use plsql.el for editing PL/SQL files, you can compile
;;      such sources everytime with C-cC-c; error messages can be parsed
;;      and displayed for easy source navigation
;;
;;  The following commands should be added to a global initialization
;;  file or to any user's .emacs file to conveniently use
;;  sqlplus-mode:
;;
;;    (require 'sqlplus)
;;    (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))
;;
;;  M-x sqlplus will start new SQL*Plus session.
;;
;;  Use describe-mode while in sqlplus-mode for further instructions.
;;
;;  Many useful commands are defined in orcl-mode minor mode, which is
;;  common for input and otput SQL*Plus buffers, as well as PL/SQL
;;  buffers.
;;
;;  For twiddling, see 'sqlplus' customization group.
;;
;;  If you find this package useful, send me a postcard to address:
;;
;;    Piotr Karpiuk
;;    Scott Tiger S.A.
;;    ul. Gawinskiego 8
;;    01-645 Warsaw
;;    Poland

;;; Known bugs:

;; 1. Result of SQL select command can be messed up if some columns
;;    has newline characters.  To avoid this, execute SQL*Plus command
;;      column <colname> truncated
;;    before such select 

;;; Code:

(require 'recentf)
(require 'font-lock)
(require 'cl)
(require 'sql)
(require 'tabify)
(require 'skeleton)

(defconst sqlplus-revision "$Revision: 1.7 $")

;;;  Variables -

(defgroup sqlplus nil
  "SQL*Plus"
  :group 'tools
  :version 21)

(defcustom plsql-auto-parse-errors-flag t
  "Non nil means parse PL/SQL compilation results and show them in the compilation buffer."
  :group 'sqlplus
  :type '(boolean))

(defcustom sqlplus-init-sequence-start-regexp "/\\* init \\*/"
  "SQL*Plus start of session init command sequence."
  :group 'sqlplus
  :type '(regexp))

(defcustom sqlplus-init-sequence-end-regexp "/\\* end \\*/"
  "SQL*Plus end of session init command sequence."
  :group 'sqlplus
  :type '(regexp))

(defcustom sqlplus-syntax-faces
  '((schema font-lock-type-face nil)
    (table font-lock-type-face ("dual"))
    (synonym font-lock-type-face nil)
    (view font-lock-type-face nil)
    (column font-lock-constant-face nil)
    (sequence font-lock-type-face nil)
    (package font-lock-type-face nil)
    (trigger font-lock-type-face nil)
    (index font-lock-type-face) nil)
  "Font lock configuration for database object names in current schema.
This is alist, and each element looks like (SYMBOL FACE LIST)
where SYMBOL is one of: schema, table, synonym, view, column,
sequence, package, trigger, index.  Database objects means only
objects from current schema, so if you want syntax highlighting
for other objects (eg. 'dual' table name), you can explicitly
enumerate them in LIST as strings."
  :group 'sqlplus
  :tag "Oracle SQL Syntax Faces"
  :type '(repeat (list symbol face (repeat string))))

(defcustom sqlplus-output-buffer-max-size (* 50 1000 1000)
  "Maximum size of SQL*Plus output buffer.
After exceeding oldest results are deleted."
  :group 'sqlplus
  :tag "SQL*Plus Output Buffer Max Size"
  :type '(integer))

(defcustom sqlplus-select-result-max-col-width nil
  "Maximum width of column in displayed database table, or nil if there is no limit.
If any cell value is longer, it will be cutted and terminated with ellipsis ('...')."
  :group 'sqlplus
  :tag "SQL*Plus Select Result Max Column Width"
  :type  '(choice integer (const nil)))

(defcustom sqlplus-format-output-tables-flag t
  "Non-nil means format result if it looks like database table."
  :group 'sqlplus
  :tag "SQL*Plus Format Output Table"
  :type '(boolean))

(defcustom sqlplus-kill-processes-without-query-on-exit-flag t
  "Non-nil means silently kill all SQL*Plus processes on Emacs exit."
  :group 'sqlplus
  :tag "SQL*Plus Kill Processes Without Query On Exit"
  :type '(boolean))

(defcustom sqlplus-multi-output-tables-default-flag t
  "Non-nil means render database table as set of adjacent tables so that they occupy all width of output window.
For screen space saving and user comfort."
  :group 'sqlplus
  :tag "SQL*Plus Multiple Tables In Output by Default"
  :type '(boolean))

(defcustom sqlplus-source-buffer-readonly-by-default-flag t
  "Non-nil means show database sources in read-only buffer."
  :group 'sqlplus
  :tag "SQL*Plus Source Buffer Read Only By Default"
  :type '(boolean))

(defcustom sqlplus-command "sqlplus"
  "SQL*Plus interpreter program."
  :group 'sqlplus
  :tag "SQL*Plus Command"
  :type '(string))

(defcustom sqlplus-history-dir nil
  "Directory of SQL*Plus command history (log) files, or nil (dont generate log files).
History file name has format '<connect-string>-history.txt'."
  :group 'sqlplus
  :tag "SQL*Plus History Dir"
  :type '(choice directory (const nil)))

(defcustom sqlplus-session-cache-dir nil
  "Directory of SQL*Plus input buffer files, or nil (dont save user session).
Session file name has format '<connect-string>.sqp'"
  :group 'sqlplus
  :tag "SQL*Plus History Dir"
  :type '(choice directory (const nil)))

(defcustom sqlplus-save-passwords nil
  "Non-nil means save passwords between Emacs sessions. (Not implemented yet)."
  :group 'sqlplus
  :tag "SQL*Plus Save Passwords"
  :type '(boolean))

(defcustom sqlplus-pagesize 200
  "Approximate number of records in query results.
If result has more rows, it will be cutted and terminated with '. . .' line."
  :group 'sqlplus
  :tag "SQL*Plus Max Rows Count"
  :type '(integer))

(defvar sqlplus-default-wrap "on")

(defcustom sqlplus-initial-strings
  (list "set sqlnumber off"
        "set tab off"
	"set linesize 4000"
        "set echo off"
        "set newpage 1"
        "set space 1"
        "set feedback 6"
        "set heading on"
        "set trimspool off"
        (format "set wrap %s" sqlplus-default-wrap)
        "set timing on"
	"set feedback on")
  "Initial commands to send to interpreter.
Customizing this variable is dangerous."
  :group 'sqlplus
  :tag "SQL*Plus Initial Strings"
  :type '(repeat string))

(defcustom sqlplus-table-col-separator " | "
  "Database table column separator (text-only terminals)."
  :group 'sqlplus
  :tag "SQL*Plus Table Col Separator"
  :type '(string))

(defcustom sqlplus-table-col-head-separator "-+-"
  "Database table header-column separator (text-only terminals)."
  :group 'sqlplus
  :tag "SQL*Plus Table Col Separator"
  :type '(string))

(defcustom sqlplus-html-output-file-name "$HOME/sqlplus_report.html"
  "Output file for HTML result."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output File Name"
  :type '(file))

(defcustom sqlplus-html-output-encoding "iso-8859-1"
  "Encoding for SQL*Plus HTML output."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Encoding"
  :type '(string))

(defcustom sqlplus-html-output-sql t
  "Non-nil means put SQL*Plus command in head of HTML result."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Encoding"
  :type '(choice (const :tag "Elegant" 'elegant)
                 (const :tag "Simple" t)
                 (const :tag "No" nil)))

(defcustom sqlplus-html-output-header (concat (current-time-string) "<br><br>")
  "HTML header sexp (result must be string)."
  :group 'sqlplus
  :tag "SQL*Plus HTML Output Header"
  :type '(sexp))
  


(defvar sqlplus-elegant-style window-system)

(defvar sqlplus-cs nil)

(defun sqlplus-shine-color (color percent)
  (when (equal color "unspecified-bg")
    (setq color (if (< percent 0) "white" "black")))
  (apply 'format "#%02x%02x%02x" 
         (mapcar (lambda (value)
                   (min 65535 (max 0 (* (+ (/ value 650) percent) 650))))
                 (color-values color))))

(defvar sqlplus-table-head-face 'sqlplus-table-head-face)
(defface sqlplus-table-head-face
  (list 
   (list '((class mono))
         '(:inherit default :weight bold :inverse-video t))
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -70) :foreground (face-background 'default))
                 (when (and sqlplus-elegant-style (>= emacs-major-version 22)) '(:box (:style released-button)))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +70) :foreground (face-background 'default))
                 (when (and sqlplus-elegant-style (>= emacs-major-version 22)) '(:box (:style released-button)))))
   '(t (:inherit 'default)))
  "Face for table header")

(defvar sqlplus-table-even-rows-face 'sqlplus-table-even-rows-face)
(defface sqlplus-table-even-rows-face
  (list 
   '(((class mono)) (:inherit default))
   '(((type tty)) (:inherit default))
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -20) :overline (face-background 'default))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +20) :overline (face-background 'default))))
   '(t (:inherit 'default)))
  "Face for table even rows")

(defvar sqlplus-table-odd-rows-face 'sqlplus-table-odd-rows-face)
(defface sqlplus-table-odd-rows-face
  (list 
   '(((class mono)) (:inherit default))
   (list '((background light))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) -30) :overline (face-background 'default))))
   (list '((background dark))
         (append (list :inherit 'default :background (sqlplus-shine-color (face-background 'default) +30) :overline (face-background 'default))))
   '(t (:inherit 'default)))
  "Face for table even rows")

(defvar sqlplus-plsql-compilation-results-buffer-name "*PL/SQL Compilation*")

(defvar sqlplus-fan " |")
(make-variable-buffer-local 'sqlplus-fan)

(defvar orcl-mode-map nil
  "Keymap used in Orcl mode.")

(define-minor-mode orcl-mode
  "Mode for executing SQL*Plus commands and scrolling results.

Mode Specific Bindings:

\\{orcl-mode-map}"
  nil                                   ; init value
  ((:eval sqlplus-fan) (:eval (connect-string-to-string))) ; mode indicator
  orcl-mode-map                                           ; keymap
  ;; body
  (setq sqlplus-fan " |")
  (unless (assq 'orcl-mode minor-mode-map-alist)
    (push (cons 'orcl-mode orcl-mode-map) minor-mode-map-alist)))

(defvar sqlplus-user-variables (makehash 'equal))

(defvar sqlplus-user-variables-history nil)

(defvar sqlplus-get-source-history nil)

(defvar sqlplus-process-p nil
  "Non-nil if current buffer is SQL*Plus process buffer.")
(make-variable-buffer-local 'sqlplus-process-p)

(defvar sqlplus-command-seq 0
  "Command id within SQL*Plus connection.")
(make-variable-buffer-local 'sqlplus-command-seq)

(defvar sqlplus-command-contexts nil
  "Command options list, for current and enqueued commands, in chronological order.")
(make-variable-buffer-local 'sqlplus-command-contexts)

(defvar sqlplus-connect-string nil
  "Local variable with connect-string for current buffer.")
(make-variable-buffer-local 'sqlplus-connect-string)

(defvar sqlplus-connect-strings-alist nil
  "Connect strings in format (CS . PASSWD), where PASSWD can be nil.")

(defvar sqlplus-connect-string-history nil)

(defvar sqlplus-prompt-prefix "SQL[")
(defvar sqlplus-prompt-suffix "]# ")

(defvar sqlplus-page-separator "@!%#!")

(defvar sqlplus-repfooter "##%@!")

(defvar sqlplus-mode-map nil
  "Keymap used in SQL*Plus mode.")

(defvar sqlplus-output-separator "@--"
  "String printed between sets of SQL*Plus command output.")

;;;  Markers -

(defvar sqlplus-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL*Plus output buffer.")
(make-variable-buffer-local 'sqlplus-buffer-mark)

(defvar sqlplus-region-beginning-pos nil
  "Marks the beginning of the region to sent to the SQL*Plus process.")
(make-variable-buffer-local 'sqlplus-region-beginning-pos)

(defvar sqlplus-region-end-pos nil
  "Marks the end of the region to sent to the SQL*Plus process.")
(make-variable-buffer-local 'sqlplus-region-end-pos)

(defvar sqlplus-connections-menu
  '("SQL*Plus"
    :filter sqlplus-connections-menu)
  "Menu for database connections")

(defconst sqlplus-kill-xpm "\
/* XPM */
static char * sql_kill_xpm[] = {
\"18 24 121 2\",
\"    c None\",    \".   c #A61410\", \"+   c #B00E00\", \"@   c #B51200\", \"#   c #B51300\", \"$   c #B51400\", \"%   c #B51600\",
\"&   c #B51700\", \"*   c #B51800\", \"=   c #AD1503\", \"-   c #E43001\", \";   c #FF4500\", \">   c #FF4A00\", \",   c #FF4E00\",
\"'   c #FF5300\", \")   c #FF5800\", \"!   c #FF5E00\", \"~   c #E94E01\", \"{   c #CA3418\", \"]   c #FD4D00\", \"^   c #FF6200\",
\"/   c #FF6800\", \"(   c #FD6A00\", \"_   c #DD4701\", \":   c #FF5D00\", \"<   c #FF6100\", \"[   c #FF6400\", \"}   c #FF6700\",
\"|   c #FF6A00\", \"1   c #FF6D00\", \"2   c #E85601\", \"3   c #B6300A\", \"4   c #FE6600\", \"5   c #FF6600\", \"6   c #FF7000\",
\"7   c #FF7300\", \"8   c #FF7700\", \"9   c #FB7700\", \"0   c #E95B01\", \"a   c #FF7500\", \"b   c #FF7B00\", \"c   c #FF8000\",
\"d   c #FF8600\", \"e   c #FF8B00\", \"f   c #FE8E00\", \"g   c #D8600B\", \"h   c #B12818\", \"i   c #D0613D\", \"j   c #CA3E03\",
\"k   c #FC7C00\", \"l   c #FF9000\", \"m   c #FF9500\", \"n   c #FF9A00\", \"o   c #EC8202\", \"p   c #DE5303\", \"q   c #EA6402\",
\"r   c #D34C04\", \"s   c #C23701\", \"t   c #F47B00\", \"u   c #FF8F00\", \"v   c #E67000\", \"w   c #DA6402\", \"x   c #F69400\",
\"y   c #FFA300\", \"z   c #FCA200\", \"A   c #C34C0A\", \"B   c #ED7401\", \"C   c #FF8D00\", \"D   c #FF9100\", \"E   c #FF9400\",
\"F   c #FF9800\", \"G   c #F08400\", \"H   c #9F2317\", \"I   c #BC584E\", \"J   c #BE5A1B\", \"K   c #D68101\", \"L   c #C96D02\",
\"M   c #CA6731\", \"N   c #FC9300\", \"O   c #FF9D00\", \"P   c #FF9F00\", \"Q   c #FFA200\", \"R   c #AD4F39\", \"S   c #AE6241\",
\"T   c #9C322C\", \"U   c #D25E09\", \"V   c #FDA000\", \"W   c #FFA800\", \"X   c #FFAE00\", \"Y   c #FFB200\", \"Z   c #D37833\",
\"`   c #DD7702\", \" .  c #FDB100\", \"..  c #FFB800\", \"+.  c #FFBD00\", \"@.  c #E08405\", \"#.  c #CB7552\", \"$.  c #EE9E01\",
\"%.  c #FFC200\", \"&.  c #FFC600\", \"*.  c #ECA602\", \"=.  c #C26938\", \"-.  c #F2BB00\", \";.  c #FFD000\", \">.  c #F6C001\",
\",.  c #B76105\", \"'.  c #FBCE00\", \").  c #FFDA00\", \"!.  c #C47B07\", \"~.  c #DD9702\", \"{.  c #FEE700\", \"].  c #D69005\",
\"^.  c #C97D6D\", \"/.  c #E9C204\", \"(.  c #ECCB01\", \"_.  c #BE621C\", \":.  c #EDD101\", \"<.  c #B85214\", \"[.  c #C87C10\",
\"}.  c #BB6410\", \"|.  c #97471A\",  
\"      . + @ # $ % & * =             \",
\"        - ; > , ' ) ! ~             \",
\"        { ] ' ) ! ^ / (             \",
\"          _ : < [ } | 1 2           \",
\"          3 4 ^ 5 6 7 8 9           \",
\"            0 a b c d e f g         \",
\"    h i     j k d e l m n o         \",
\"      p q r s t u v w x y z A       \",
\"        B C D E F G H I J K L       \",
\"        M N n O P Q R       S T     \",
\"          U V W X Y Z               \",
\"            `  ...+.@.              \",
\"            #.$.%.&.*.              \",
\"              =.-.;.>.              \",
\"                ,.'.).!.            \",
\"                  ~.{.].            \",
\"                  ^./.(.            \",
\"                    _.:.<.          \",
\"                      [.}.          \",
\"                        |.          \",
\"                                    \",
\"                                    \",
\"                                    \",
\"                                    \"};
\"
"
  "XPM format image used as Kill icon")

(defconst sqlplus-cancel-xpm "\
/* XPM */
static char * sql_cancel_xpm[] = {
\"24 24 232 2\",
\"   c None\",    \".  c #FA6B2C\", \"+  c #F96829\", \"@  c #F96729\", \"#  c #F86529\", \"$  c #F76429\", \"%  c #F56229\",
\"&  c #F45F29\", \"*  c #F35E29\", \"=  c #F25E2A\", \"-  c #EF6033\", \";  c #FB7D45\", \">  c #F04C18\", \",  c #E43C11\",
\"'  c #E13910\", \")  c #E0370F\", \"!  c #E03810\", \"~  c #DF370F\", \"{  c #DE3610\", \"]  c #DF3812\", \"^  c #E03A15\",
\"/  c #DF3D1A\", \"(  c #DF401C\", \"_  c #E3421D\", \":  c #E73E16\", \"<  c #FA743F\", \"[  c #E63B0F\", \"}  c #BE1603\",
\"|  c #BA1101\", \"1  c #B91000\", \"2  c #BA1100\", \"3  c #BB1200\", \"4  c #BC1200\", \"5  c #BE1301\", \"6  c #BE1300\",
\"7  c #BF1401\", \"8  c #C11501\", \"9  c #C21803\", \"0  c #C41D09\", \"a  c #C72410\", \"b  c #CA2916\", \"c  c #CC301B\",
\"d  c #D13620\", \"e  c #D7381E\", \"f  c #DE2C0B\", \"g  c #F76838\", \"h  c #EE4111\", \"i  c #BE1502\", \"j  c #B10C00\",
\"k  c #BC2B20\", \"l  c #C02D20\", \"m  c #BA1000\", \"n  c #BB1100\", \"o  c #BD1301\", \"p  c #BD1402\", \"q  c #C11A08\",
\"r  c #D03D29\", \"s  c #D13D29\", \"t  c #D93014\", \"u  c #C41F0A\", \"v  c #F4420F\", \"w  c #E12F08\", \"x  c #BB2B20\",
\"y  c #C43223\", \"z  c #BE1705\", \"A  c #BF1A09\", \"B  c #D2422E\", \"C  c #D53016\", \"D  c #BE1501\", \"E  c #F5430F\",
\"F  c #BD1605\", \"G  c #BE1401\", \"H  c #C01906\", \"I  c #C4200E\", \"J  c #D34532\", \"K  c #D02E17\", \"L  c #BB1400\",
\"M  c #F4410F\", \"N  c #E02E08\", \"O  c #CA3929\", \"P  c #C31F0D\", \"Q  c #D64B38\", \"R  c #D1311B\", \"S  c #BA1300\",
\"T  c #D7503D\", \"U  c #D1331C\", \"V  c #B71300\", \"W  c #F2400F\", \"X  c #B80F00\", \"Y  c #C22E20\", \"Z  c #D95643\",
\"`  c #D95441\", \" . c #D2341E\", \".. c #B61200\", \"+. c #F3400F\", \"@. c #BF1400\", \"#. c #DC5F4C\", \"$. c #DD614E\",
\"%. c #DD604D\", \"&. c #D33923\", \"*. c #B31200\", \"=. c #F03E0F\", \"-. c #DE2D08\", \";. c #BC1100\", \">. c #BD1200\",
\",. c #C73222\", \"'. c #D85441\", \"). c #DA5A46\", \"!. c #DD604E\", \"~. c #DD5D4A\", \"{. c #D43A23\", \"]. c #B21101\",
\"^. c #EF3D0F\", \"/. c #BF1402\", \"(. c #C21A08\", \"_. c #D95744\", \":. c #DA5946\", \"<. c #DF6553\", \"[. c #DF614E\",
\"}. c #D43A24\", \"|. c #AF1100\", \"1. c #EE3D0F\", \"2. c #DE2C08\", \"3. c #C11400\", \"4. c #C11907\", \"5. c #E27666\",
\"6. c #E26B58\", \"7. c #E06653\", \"8. c #D43D28\", \"9. c #AE1100\", \"0. c #ED3C0F\", \"a. c #BE1200\", \"b. c #C01805\",
\"c. c #E67F6E\", \"d. c #E4715D\", \"e. c #E36B57\", \"f. c #D53E28\", \"g. c #AD1100\", \"h. c #DF6653\", \"i. c #DE6552\",
\"j. c #D43B24\", \"k. c #B21200\", \"l. c #EA3B10\", \"m. c #DC2C09\", \"n. c #C41803\", \"o. c #D74E3B\", \"p. c #E47463\",
\"q. c #D5422D\", \"r. c #A90F00\", \"s. c #DC2D0A\", \"t. c #C61D08\", \"u. c #D95340\", \"v. c #E0705F\", \"w. c #E67866\",
\"x. c #D6442E\", \"y. c #A80E00\", \"z. c #E9390F\", \"A. c #DC2F0D\", \"B. c #C7230D\", \"C. c #D74E3A\", \"D. c #DB5946\",
\"E. c #E3796A\", \"F. c #E77D6A\", \"G. c #D54631\", \"H. c #A70E00\", \"I. c #E43E1B\", \"J. c #DD2E0D\", \"K. c #CB2712\",
\"L. c #CA2E1B\", \"M. c #D64D39\", \"N. c #D85340\", \"O. c #DC5946\", \"P. c #DE5F4B\", \"Q. c #E26C59\", \"R. c #E4725E\",
\"S. c #EA8978\", \"T. c #E77C68\", \"U. c #D4402A\", \"V. c #9E1005\", \"W. c #D62405\", \"X. c #CE2810\", \"Y. c #CD2D17\",
\"Z. c #CF301A\", \"`. c #D0331E\", \" + c #D1351F\", \".+ c #D23923\", \"++ c #D33C26\", \"@+ c #D53F2A\", \"#+ c #D6422C\",
\"$+ c #D7452F\", \"%+ c #D84934\", \"&+ c #D94C36\", \"*+ c #DA4F3A\", \"=+ c #DA513C\", \"-+ c #DC5540\", \";+ c #DC5843\",
\">+ c #DD5944\", \",+ c #DA4C35\", \"'+ c #CD2810\", \")+ c #973836\", \"!+ c #D01F04\", \"~+ c #D3260B\", \"{+ c #D0260D\",
\"]+ c #CF280F\", \"^+ c #CF2811\", \"/+ c #D02A12\", \"(+ c #D02B13\", \"_+ c #CF2C15\", \":+ c #CF2E17\", \"<+ c #D02F19\",
\"[+ c #D0311A\", \"}+ c #CF321C\", \"|+ c #CF321D\", \"1+ c #D0341E\", \"2+ c #CF3520\", \"3+ c #CC2E19\", \"4+ c #BC1B07\",
\"5+ c #980C00\", \"6+ c #B0170A\", \"7+ c #AE0F00\", \"8+ c #AD0E00\", \"9+ c #AC0E00\", \"0+ c #A90E00\", \"a+ c #A70D00\",
\"b+ c #A50D00\", \"c+ c #A20C00\", \"d+ c #A10C00\", \"e+ c #9D0B00\", \"f+ c #9A0B00\", \"g+ c #980B00\", \"h+ c #8D0A03\",
\"i+ c #8D2725\",
\"        . + @ # # $ % % % % & * = = -           \",
\"    ; > , ' ) ) ) ! ~ { { { ] ^ / ( _ :         \",
\"  < [ } | 1 2 3 4 5 6 7 8 9 0 a b c d e f       \",
\"g h i j k     l 1 m n 4 o p q       r s t u     \",
\"v w 1 x         y n 4 o z A           B C D     \",
\"E w 2             F G H I             J K L     \",
\"M N 3               O P               Q R S     \",
\"M N 4 l                               T U V     \",
\"W N 4 X Y                           Z `  ...    \",
\"+.N @.n n n                       #.$.%.&.*.    \",
\"=.-.6 2 ;.>.,.                  '.).!.~.{.].    \",
\"^.-.@.;.>./.(.                _.:.#.<.[.}.|.    \",
\"1.2.3.>.G 4.                    5.<.6.7.8.9.    \",
\"0.2.3.a.b.                        c.d.e.f.g.    \",
\"W N 3.>.                            h.i.j.k.    \",
\"l.m.n.                o.              p.q.r.    \",
\"l.s.t.              o.u.v.            w.x.y.    \",
\"z.A.B.            C.u.D.%.E.          F.G.H.    \",
\"I.J.K.L.        M.N.O.P.7.Q.R.      S.T.U.V.    \",
\"  W.X.Y.Z.`. +.+++@+#+$+%+&+*+=+-+;+>+,+'+)+    \",
\"    !+~+{+]+^+/+(+_+:+:+<+[+}+|+1+2+3+4+5+      \",
\"      6+7+8+9+0+a+b+c+d+e+e+e+f+g+g+h+i+        \",
\"                                                \",
\"                                                \"};
"
  "XPM format image used as Cancel icon")

(defconst sqlplus-rollback-xpm "\
/* XPM */
static char * sql_rollback_xpm[] = {
\"24 24 321 2\",
\"   c None\",    \".  c #D5CC6F\", \"+  c #CAC265\", \"@  c #D0BD50\", \"#  c #D4BA43\", \"$  c #D3B940\", \"%  c #C7AA40\",
\"&  c #C0A552\", \"*  c #D6D485\", \"=  c #E5E184\", \"-  c #F8F080\", \";  c #FEF57B\", \">  c #FFF571\", \",  c #FFF164\",
\"'  c #FFED58\", \")  c #FFE748\", \"!  c #FEDE39\", \"~  c #F1CF29\", \"{  c #D1AD29\", \"]  c #BC982F\", \"^  c #D9D371\",
\"/  c #F8F897\", \"(  c #FFFE96\", \"_  c #FFFA8A\", \":  c #FFF67C\", \"<  c #FFF16E\", \"[  c #FFEC62\", \"}  c #FFE956\",
\"|  c #FFE448\", \"1  c #FFE03C\", \"2  c #FFDD30\", \"3  c #FED821\", \"4  c #F1CB15\", \"5  c #BB9406\", \"6  c #A3954F\",
\"7  c #FFFC92\", \"8  c #FFFC91\", \"9  c #FFFC90\", \"0  c #FFFB8D\", \"a  c #FFF67D\", \"b  c #FFEB5E\", \"c  c #FFEA5B\",
\"d  c #FFE958\", \"e  c #FFE855\", \"f  c #FFE752\", \"g  c #FDD41C\", \"h  c #FDD319\", \"i  c #FDD416\", \"j  c #6E4E1A\",
\"k  c #C6BA61\", \"l  c #FFFF9D\", \"m  c #FFFF99\", \"n  c #FFFD94\", \"o  c #FFFA89\", \"p  c #FFDC2F\", \"q  c #FED315\",
\"r  c #FFD808\", \"s  c #976E09\", \"t  c #BFAE58\", \"u  c #FFFC9F\", \"v  c #FFFE99\", \"w  c #FFDF3B\", \"x  c #F7C909\",
\"y  c #845B0B\", \"z  c #BA9C3C\", \"A  c #F8EA86\", \"B  c #FEFCB7\", \"C  c #FFFDA6\", \"D  c #FFFA91\", \"E  c #FFF681\",
\"F  c #FFF171\", \"G  c #FFED64\", \"H  c #FFE44A\", \"I  c #FFE03D\", \"J  c #FEDB2F\", \"K  c #F9D21E\", \"L  c #E9BC0F\",
\"M  c #CE9C02\", \"N  c #804D0E\", \"O  c #B6973D\", \"P  c #F3E36A\", \"Q  c #FCF899\", \"R  c #FFFCA3\", \"S  c #FEF694\",
\"T  c #FFF284\", \"U  c #FFEE71\", \"V  c #FFEA62\", \"W  c #FDDC40\", \"X  c #F8D22F\", \"Y  c #F1C61B\", \"Z  c #DDAD0A\",
\"`  c #CC9A02\", \" . c #C89500\", \".. c #784C13\", \"+. c #9D8943\", \"@. c #F4EA77\", \"#. c #F7EF7F\", \"$. c #FFF16A\",
\"%. c #FFEF68\", \"&. c #FFEE66\", \"*. c #FED622\", \"=. c #FED51E\", \"-. c #FED419\", \";. c #E9B90E\", \">. c #E7B509\",
\",. c #D4A202\", \"'. c #CA9700\", \"). c #6A450C\", \"!. c #C2A953\", \"~. c #F6E67C\", \"{. c #F3E67F\", \"]. c #FCEE7A\",
\"^. c #FDEB66\", \"/. c #FEE44E\", \"(. c #FED313\", \"_. c #FDCA03\", \":. c #F2BE01\", \"<. c #D4A60D\", \"[. c #D4A206\",
\"}. c #D19C00\", \"|. c #CF9800\", \"1. c #E3AF02\", \"2. c #7A5B1D\", \"3. c #4E5B90\", \"4. c #36447E\", \"5. c #434E84\",
\"6. c #BC9F3D\", \"7. c #F9EB81\", \"8. c #FBF096\", \"9. c #F9E67C\", \"0. c #F8DC5F\", \"a. c #F8D548\", \"b. c #F9D02D\",
\"c. c #F9C915\", \"d. c #F7C104\", \"e. c #EEB606\", \"f. c #8F7336\", \"g. c #6F5B40\", \"h. c #E9B704\", \"i. c #DEAE08\",
\"j. c #9B7E2D\", \"k. c #414D7B\", \"l. c #3C5CA2\", \"m. c #3A65B3\", \"n. c #3668BB\", \"o. c #325EAF\", \"p. c #314E94\",
\"q. c #F3E46E\", \"r. c #FCFA9B\", \"s. c #FFF89C\", \"t. c #FDEC81\", \"u. c #FCE668\", \"v. c #FDDF4E\", \"w. c #FCDA3C\",
\"x. c #FCD52E\", \"y. c #FAD026\", \"z. c #978043\", \"A. c #4662A2\", \"B. c #7B6841\", \"C. c #897037\", \"D. c #465A8D\",
\"E. c #3F6CBA\", \"F. c #3A68B7\", \"G. c #375396\", \"H. c #3A4C89\", \"I. c #2E529E\", \"J. c #2655AC\", \"K. c #354983\",
\"L. c #B99C3F\", \"M. c #F0DC69\", \"N. c #FBF78C\", \"O. c #FFF880\", \"P. c #FFF06B\", \"Q. c #FFE03E\", \"R. c #FFD828\",
\"S. c #FED015\", \"T. c #F5C40A\", \"U. c #917836\", \"V. c #4B70B4\", \"W. c #4870B7\", \"X. c #3C5CA1\", \"Y. c #4070BF\",
\"Z. c #3759A0\", \"`. c #3A447B\", \" + c #37427A\", \".+ c #1D469C\", \"++ c #214493\", \"@+ c #3A3C6E\", \"#+ c #AC9946\",
\"$+ c #F2DD6C\", \"%+ c #F8EB7E\", \"&+ c #FBEE7A\", \"*+ c #FBE461\", \"=+ c #FADB48\", \"-+ c #FBD631\", \";+ c #FED10F\",
\">+ c #FECD07\", \",+ c #F1BD00\", \"'+ c #8D732C\", \")+ c #456AAE\", \"!+ c #4C7ECA\", \"~+ c #487AC8\", \"{+ c #35528F\",
\"]+ c #38344E\", \"^+ c #2D2F5A\", \"/+ c #1B4294\", \"(+ c #1B4193\", \"_+ c #2C2D59\", \":+ c #B2983F\", \"<+ c #F9EA83\",
\"[+ c #FCF08E\", \"}+ c #F6E16E\", \"|+ c #F4D559\", \"1+ c #F5CF45\", \"2+ c #F6CB2E\", \"3+ c #F8C611\", \"4+ c #F6C005\",
\"5+ c #E8B300\", \"6+ c #896F30\", \"7+ c #4268AE\", \"8+ c #4375C4\", \"9+ c #3F71C1\", \"0+ c #33569B\", \"a+ c #353259\",
\"b+ c #373B6B\", \"c+ c #173F94\", \"d+ c #183A8B\", \"e+ c #363967\", \"f+ c #B79A3D\", \"g+ c #F3E36E\", \"h+ c #FCF7A1\",
\"i+ c #FEF9A1\", \"j+ c #FEEE7D\", \"k+ c #FCE360\", \"l+ c #FAD946\", \"m+ c #F9D132\", \"n+ c #F8CD26\", \"o+ c #F7CA20\",
\"p+ c #967F41\", \"q+ c #3B589A\", \"r+ c #395FA9\", \"s+ c #3359A5\", \"t+ c #3056A3\", \"u+ c #2B468D\", \"v+ c #333366\",
\"w+ c #223A85\", \"x+ c #0A3897\", \"y+ c #2A3D80\", \"z+ c #AF9146\", \"A+ c #E6D465\", \"B+ c #FDFA90\", \"C+ c #FFF885\",
\"D+ c #FFF074\", \"E+ c #FFEA60\", \"F+ c #FFE246\", \"G+ c #FFDC31\", \"H+ c #FED51F\", \"I+ c #F7CB14\", \"J+ c #C29E21\",
\"K+ c #8A7033\", \"L+ c #836833\", \"M+ c #6A4F36\", \"N+ c #454C80\", \"O+ c #173788\", \"P+ c #063494\", \"Q+ c #353A71\",
\"R+ c #BBA54A\", \"S+ c #E8DE7B\", \"T+ c #FFFA86\", \"U+ c #FFF26A\", \"V+ c #FFE84F\", \"W+ c #FFD415\", \"X+ c #FDCC04\",
\"Y+ c #F3C001\", \"Z+ c #EBB600\", \"`+ c #E3AF01\", \" @ c #D7A100\", \".@ c #AF7904\", \"+@ c #757699\", \"@@ c #2D3E7F\",
\"#@ c #033396\", \"$@ c #1E3782\", \"%@ c #A6906A\", \"&@ c #B9A84F\", \"*@ c #CFB954\", \"=@ c #DBC347\", \"-@ c #DEBF2C\",
\";@ c #DFB718\", \">@ c #DFB206\", \",@ c #D6A505\", \"'@ c #C6970A\", \")@ c #B48413\", \"!@ c #89591B\", \"~@ c #666789\",
\"{@ c #374682\", \"]@ c #023398\", \"^@ c #0E3287\", \"/@ c #3A4274\", \"(@ c #766A49\", \"_@ c #756332\", \":@ c #7C5D1A\",
\"<@ c #7E5913\", \"[@ c #80560D\", \"}@ c #744B16\", \"|@ c #664729\", \"1@ c #6E544C\", \"2@ c #404775\", \"3@ c #253775\",
\"4@ c #05318F\", \"5@ c #10358B\", \"6@ c #1F3170\", \"7@ c #464973\", \"8@ c #183888\", \"9@ c #053495\", \"0@ c #0E348D\",
\"a@ c #343C75\", \"b@ c #3C3F71\", \"c@ c #183585\", \"d@ c #223880\", \"e@ c #4D5181\", \"f@ c #393B6D\", 
\"        . + @ # $ % &                           \",
\"    * = - ; > , ' ) ! ~ { ]                     \",
\"  ^ / ( _ : < [ } | 1 2 3 4 5                   \",
\"6 7 8 9 0 _ a b c d e f g h i j                 \",
\"k l m n o : < [ } | 1 p 3 q r s                 \",
\"t u v n o : < [ } | w p 3 q x y                 \",
\"z A B C D E F G d H I J K L M N                 \",
\"O P Q R S T U V f W X Y Z `  ...                \",
\"+.@.#.$.%.&.*.=.-.;.>.,.'. . .).                \",
\"!.~.{.].^./.J (._.:.<.[.}.|.1.2.  3.4.5.        \",
\"6.7.8.9.0.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.      \",
\"O q.r.s.t.u.v.w.x.y.z.A.B.C.D.E.F.G.H.I.J.K.    \",
\"L.M.N.O.P.} Q.R.S.T.U.V.W.X.Y.Z.`.     +.+++@+  \",
\"#+$+%+&+*+=+-+;+>+,+'+)+!+~+{+]+      ^+/+(+_+  \",
\":+<+[+}+|+1+2+3+4+5+6+7+8+9+0+a+      b+c+d+e+  \",
\"f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+    w+x+y+    \",
\"z+A+B+C+D+E+F+G+H+I+J+K+L+L+M+v+v+  N+O+P+Q+    \",
\"  R+S+T+U+V+2 W+X+Y+Z+`+ @.@      +@@@#@$@      \",
\"    %@&@*@=@-@;@>@,@'@)@!@      ~@{@]@^@/@      \",
\"        (@_@:@<@[@}@|@1@      2@3@4@5@6@        \",
\"                            7@8@9@0@a@          \",
\"                            b@c@d@e@            \",
\"                              f@                \",
\"                                                \"};
"
  "XPM format image used as Rollback icon")

(defconst sqlplus-commit-xpm "\
/* XPM */
static char * sql_commit_xpm[] = {
\"24 24 363 2\",
\"   c None\",     \".  c #C9C269\",  \"+  c #C9BA58\",  \"@  c #D0B745\",  \"#  c #D1B53F\",  \"$  c #CCAF3D\",  \"%  c #C2A544\",
\"&  c #B99F5A\",  \"*  c #D0CE81\",  \"=  c #D9D783\",  \"-  c #F4EA7D\",  \";  c #FDF57D\",  \">  c #FFF676\",  \",  c #FFF36C\",
\"'  c #FFF05D\",  \")  c #FFEB51\",  \"!  c #FFE445\",  \"~  c #FDDC35\",  \"{  c #E3C025\",  \"]  c #C8A735\",  \"^  c #B79431\",
\"/  c #D1CA68\",  \"(  c #EFEA85\",  \"_  c #FBF68D\",  \":  c #FCF482\",  \"<  c #FCF178\",  \"[  c #FCEE6E\",  \"}  c #FCEB66\",
\"|  c #FCE85B\",  \"1  c #FCE551\",  \"2  c #FDE147\",  \"3  c #FDDF3D\",  \"4  c #FEDD2D\",  \"5  c #FCD621\",  \"6  c #E5BF16\",
\"7  c #B3890B\",  \"8  c #87773D\",  \"9  c #D8D479\",  \"0  c #FCF587\",  \"a  c #FAEF78\",  \"b  c #FAEA6B\",  \"c  c #FAEA6A\",
\"d  c #FAE968\",  \"e  c #FAE967\",  \"f  c #FAE865\",  \"g  c #FAE864\",  \"h  c #FDDD3C\",  \"i  c #FED621\",  \"j  c #FFD51D\",
\"k  c #FFD51B\",  \"l  c #FFD519\",  \"m  c #D8B82B\",  \"n  c #5E421F\",  \"o  c #C2B45A\",  \"p  c #FCF790\",  \"q  c #FBF587\",
\"r  c #F8EF7D\",  \"s  c #F8EC75\",  \"t  c #F7E86B\",  \"u  c #F8E868\",  \"v  c #F9E663\",  \"w  c #F9E45A\",  \"x  c #F9E253\",
\"y  c #F9E04C\",  \"z  c #FBDD40\",  \"A  c #FBDB38\",  \"B  c #FAD933\",  \"C  c #FAD529\",  \"D  c #FDD810\",  \"E  c #A0740A\",
\"F  c #BEAF57\",  \"G  c #FFFD9E\",  \"H  c #FFFF9A\",  \"I  c #FFFE96\",  \"J  c #FFFB8C\",  \"K  c #FFF781\",  \"L  c #FFF375\",
\"M  c #FFEF69\",  \"N  c #FFEA5B\",  \"O  c #FFE750\",  \"P  c #FFE345\",  \"Q  c #FFDF38\",  \"R  c #FFDB2B\",  \"S  c #FFD81F\",
\"T  c #FFD313\",  \"U  c #FBD007\",  \"V  c #8E5F0E\",  \"W  c #B89E41\",  \"X  c #FBF090\",  \"Y  c #FFFDAE\",  \"Z  c #FFFEA2\",
\"`  c #FFFA8C\",  \" . c #FFF780\",  \".. c #F6CA11\",  \"+. c #E1AF03\",  \"@. c #81510F\",  \"#. c #B4963A\",  \"$. c #F4E36D\",
\"%. c #FCF7A4\",  \"&. c #FFFEBB\",  \"*. c #FEFAA6\",  \"=. c #FFF990\",  \"-. c #FFF57E\",  \";. c #FFEE6F\",  \">. c #FFEB61\",
\",. c #FFE856\",  \"'. c #FFE34A\",  \"). c #FBDD44\",  \"!. c #F7D535\",  \"~. c #EBBF13\",  \"{. c #D5A406\",  \"]. c #C99500\",
\"^. c #7F4F0F\",  \"/. c #A98C3C\",  \"(. c #F0DC5F\",  \"_. c #F3E772\",  \":. c #F7EC76\",  \"<. c #F6E56D\",  \"[. c #F6E369\",
\"}. c #F6E264\",  \"|. c #F5DF5C\",  \"1. c #F3DB53\",  \"2. c #F3D849\",  \"3. c #EFD245\",  \"4. c #ECCE3F\",  \"5. c #E3B91F\",
\"6. c #D3A40B\",  \"7. c #C99600\",  \"8. c #C69200\",  \"9. c #734912\",  \"0. c #B51505\",  \"a. c #A18B43\",  \"b. c #EED95E\",
\"c. c #EDDA60\",  \"d. c #F1DF64\",  \"e. c #F2DF5E\",  \"f. c #F2DD57\",  \"g. c #F2D94E\",  \"h. c #F2D644\",  \"i. c #EFD038\",
\"j. c #ECCB34\",  \"k. c #E6C430\",  \"l. c #DFB71F\",  \"m. c #D9AD17\",  \"n. c #CC9907\",  \"o. c #C69000\",  \"p. c #D39E00\",
\"q. c #774F0C\",  \"r. c #AC241F\",  \"s. c #BB1503\",  \"t. c #B51406\",  \"u. c #BCA045\",  \"v. c #F9EA7D\",  \"w. c #F6E57A\",
\"x. c #F5E370\",  \"y. c #F5DE62\",  \"z. c #F9DF52\",  \"A. c #FBDB3E\",  \"B. c #FCD526\",  \"C. c #FCCE0F\",  \"D. c #F7C50A\",
\"E. c #EEBA08\",  \"F. c #E2AB03\",  \"G. c #D7A000\",  \"H. c #D59D00\",  \"I. c #DFA901\",  \"J. c #E7B402\",  \"K. c #88580F\",
\"L. c #AE1B15\",  \"M. c #C91800\",  \"N. c #B11309\",  \"O. c #B89936\",  \"P. c #F6E676\",  \"Q. c #FCF4A1\",  \"R. c #FDF096\",
\"S. c #FAE167\",  \"T. c #F7D64F\",  \"U. c #F7CF38\",  \"V. c #F7CB26\",  \"W. c #F6BF0C\",  \"X. c #F1B905\",  \"Y. c #ECB309\",
\"Z. c #EBB60A\",  \"`. c #F0BF0B\",  \" + c #F3C206\",  \".+ c #E5B201\",  \"++ c #CF9C01\",  \"@+ c #A01111\",  \"#+ c #C21602\",
\"$+ c #C21703\",  \"%+ c #A70F0A\",  \"&+ c #B1913C\",  \"*+ c #F2E067\",  \"=+ c #FBF78F\",  \"-+ c #FEF28A\",  \";+ c #FEED74\",
\">+ c #FFE85F\",  \",+ c #FFE24D\",  \"'+ c #FFDE3A\",  \")+ c #FED92F\",  \"!+ c #FCD325\",  \"~+ c #F8CD1A\",  \"{+ c #EDBD0A\",
\"]+ c #D9A701\",  \"^+ c #C79200\",  \"/+ c #794C13\",  \"(+ c #B61407\",  \"_+ c #D11D00\",  \":+ c #B3180C\",  \"<+ c #B79D3F\",
\"[+ c #EFDA64\",  \"}+ c #F7EF7F\",  \"|+ c #FCF47F\",  \"1+ c #FDEE6C\",  \"2+ c #FDE85B\",  \"3+ c #FDE249\",  \"4+ c #FDDC36\",
\"5+ c #FCD423\",  \"6+ c #F9CC14\",  \"7+ c #F0C10E\",  \"8+ c #E6B507\",  \"9+ c #DCA900\",  \"0+ c #D29F00\",  \"a+ c #C69400\",
\"b+ c #C99200\",  \"c+ c #A80E08\",  \"d+ c #CC1B02\",  \"e+ c #C61A04\",  \"f+ c #A11412\",  \"g+ c #9D8C40\",  \"h+ c #E1CF5F\",
\"i+ c #EAD862\",  \"j+ c #ECDB63\",  \"k+ c #EFDC5E\",  \"l+ c #EFD955\",  \"m+ c #EFD74D\",  \"n+ c #EFD444\",  \"o+ c #F0D23E\",
\"p+ c #EECE37\",  \"q+ c #E8C731\",  \"r+ c #E0B922\",  \"s+ c #D09E03\",  \"t+ c #CB9700\",  \"u+ c #C39100\",  \"v+ c #C99400\",
\"w+ c #CC1D03\",  \"x+ c #E12400\",  \"y+ c #B81606\",  \"z+ c #AF9339\",  \"A+ c #F2E47C\",  \"B+ c #F8ED8C\",  \"C+ c #F4E171\",
\"D+ c #F0D65B\",  \"E+ c #F0D24F\",  \"F+ c #F1CF43\",  \"G+ c #F2CD34\",  \"H+ c #F2C824\",  \"I+ c #EEC527\",  \"J+ c #E7BD23\",
\"K+ c #DFAC12\",  \"L+ c #DAA203\",  \"M+ c #E5B202\",  \"N+ c #EDBA01\",  \"O+ c #D69F00\",  \"P+ c #804714\",  \"Q+ c #AF221B\",
\"R+ c #D21E01\",  \"S+ c #D01C00\",  \"T+ c #A82927\",  \"U+ c #B59638\",  \"V+ c #F2E16A\",  \"W+ c #FBF59D\",  \"X+ c #FEFBAA\",
\"Y+ c #FEF084\",  \"Z+ c #FCE567\",  \"`+ c #FBDD50\",  \" @ c #F8D23B\",  \".@ c #F8CD28\",  \"+@ c #EEB51C\",  \"@@ c #DA8A13\",
\"#@ c #E29A16\",  \"$@ c #EDB111\",  \"%@ c #E5AE08\",  \"&@ c #D19C01\",  \"*@ c #C79400\",  \"=@ c #932B06\",  \"-@ c #BF1603\",
\";@ c #DD2300\",  \">@ c #AC140C\",  \",@ c #AB8C47\",  \"'@ c #E6D261\",  \")@ c #FCF88C\",  \"!@ c #FFF27A\",  \"~@ c #FFEC6A\",
\"{@ c #FFE655\",  \"]@ c #FFE041\",  \"^@ c #FFDA2B\",  \"/@ c #E49D14\",  \"(@ c #AC2100\",  \"_@ c #B10E01\",  \":@ c #BE2D01\",
\"<@ c #BA4F02\",  \"[@ c #BB6A00\",  \"}@ c #B37102\",  \"|@ c #A51006\",  \"1@ c #DD2200\",  \"2@ c #CA1B02\",  \"3@ c #AA2623\",
\"4@ c #C2AB4D\",  \"5@ c #E6DB78\",  \"6@ c #FEFB8B\",  \"7@ c #FFF470\",  \"8@ c #FFEA56\",  \"9@ c #FFE13E\",  \"0@ c #FFDA24\",
\"a@ c #FECF0A\",  \"b@ c #F5BE01\",  \"c@ c #D37800\",  \"d@ c #BA1F00\",  \"e@ c #D72000\",  \"f@ c #D41F02\",  \"g@ c #C02101\",
\"h@ c #9D1D03\",  \"i@ c #C61802\",  \"j@ c #B61A0E\",  \"k@ c #B4A069\",  \"l@ c #C0B25D\",  \"m@ c #EBD55C\",  \"n@ c #FCE353\",
\"o@ c #FFE33E\",  \"p@ c #FFDB26\",  \"q@ c #FFD20B\",  \"r@ c #FCCB01\",  \"s@ c #F0B900\",  \"t@ c #D47D00\",  \"u@ c #B51100\",
\"v@ c #E42500\",  \"w@ c #EB2900\",  \"x@ c #DF2301\",  \"y@ c #E82700\",  \"z@ c #D31F04\",  \"A@ c #AC150E\",  \"B@ c #7A6259\",
\"C@ c #756332\",  \"D@ c #795F24\",  \"E@ c #7C5D1A\",  \"F@ c #7F570C\",  \"G@ c #7C520F\",  \"H@ c #6F4919\",  \"I@ c #66493A\",
\"J@ c #673D35\",  \"K@ c #830E05\",  \"L@ c #C71F01\",  \"M@ c #EA2800\",  \"N@ c #E92800\",  \"O@ c #D11F02\",  \"P@ c #B5160A\",
\"Q@ c #DD2301\",  \"R@ c #E22501\",  \"S@ c #AB0F04\",  \"T@ c #B41307\",  \"U@ c #CB1A01\",  \"V@ c #A20803\",
\"          . + @ # $ % &                         \",
\"    * = - ; > , ' ) ! ~ { ] ^                   \",
\"  / ( _ : < [ } | 1 2 3 4 5 6 7                 \",
\"8 9 0 a b c d e f g h i j k l m n               \",
\"o p q r s t u v w x y z A B C D E               \",
\"F G H I J K L M N O P Q R S T U V               \",
\"W X Y Z `  .L M N O P Q R S ..+.@.              \",
\"#.$.%.&.*.=.-.;.>.,.'.).!.~.{.].^.              \",
\"/.(._.:.<.[.}.|.1.2.3.4.5.6.7.8.9.        0.0.  \",
\"a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.      r.s.t.  \",
\"u.v.w.x.y.z.A.B.C.D.E.F.G.H.I.J.K.      L.M.N.  \",
\"O.P.Q.R.S.T.U.V.W.X.Y.Z.`. +.+++^.    @+#+$+%+  \",
\"&+*+=+G -+;+>+,+'+)+!+~+{+]+7.^+/+    (+_+:+    \",
\"<+[+}+|+1+2+3+4+5+6+7+8+9+0+a+b+K.  c+d+e+f+    \",
\"g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+q.  w+x+y+      \",
\"z+A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+      \",
\"U+V+W+X+Y+Z+`+ @.@+@@@#@$@%@&@*@=@-@;@>@        \",
\",@'@)@J !@~@{@]@^@/@(@_@:@<@[@}@|@1@2@3@        \",
\"  4@5@6@7@8@9@0@a@b@c@d@e@f@g@h@i@x+j@          \",
\"    k@l@m@n@o@p@q@r@s@t@u@v@w@x@y@z@A@          \",
\"        B@C@D@E@F@G@H@I@J@K@L@M@N@O@            \",
\"                            P@Q@R@S@            \",
\"                              T@U@              \",
\"                                V@              \"};
"
  "XPM format image used as Commit icon")

(defconst sqlplus-kill-image
  (create-image sqlplus-kill-xpm 'xpm t))

(defconst sqlplus-cancel-image
  (create-image sqlplus-cancel-xpm 'xpm t))

(defconst sqlplus-commit-image
  (create-image sqlplus-commit-xpm 'xpm t))

(defconst sqlplus-rollback-image
  (create-image sqlplus-rollback-xpm 'xpm t))

(defvar sqlplus-mode-syntax-table nil
  "Syntax table used while in sqlplus-mode.")

(defvar sqlplus-suppress-show-output-buffer nil)

(defvar sqlplus-font-lock-keywords-1 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-1)
(defvar sqlplus-font-lock-keywords-2 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-2)
(defvar sqlplus-font-lock-keywords-3 nil)
(make-variable-buffer-local 'sqlplus-font-lock-keywords-3)

(defvar sqlplus-font-lock-defaults '((sqlplus-font-lock-keywords-1 sqlplus-font-lock-keywords-2 sqlplus-font-lock-keywords-3) nil t nil nil))
(defvar sqlplus-font-lock-regexps nil)
(make-variable-buffer-local 'sqlplus-font-lock-regexps)

(defvar sqlplus-oracle-extra-builtin-functions-re
  (concat "\\b"
          (regexp-opt '("acos" "asciistr" "asin" "atan" "atan2" "bfilename" "bin_to_num" "bitand" "cardinality" "cast" "coalesce" "collect"
                        "compose" "corr" "corr_s" "corr_k" "covar_pop" "covar_samp" "cume_dist" "current_date" "current_timestamp" "cv"
                        "dbtimezone" "decompose" "dense_rank" "depth" "deref" "empty_blob, empty_clob" "existsnode" "extract"
                        "extractvalue" "first" "first_value" "from_tz" "group_id" "grouping" "grouping_id" "iteration_number"
                        "lag" "last" "last_value" "lead" "lnnvl" "localtimestamp" "make_ref" "median" "nanvl" "nchr" "nls_charset_decl_len"
                        "nls_charset_id" "nls_charset_name" "ntile" "nullif" "numtodsinterval" "numtoyminterval" "nvl2" "ora_hash" "path"
                        "percent_rank" "percentile_cont" "percentile_disc" "powermultiset" "powermultiset_by_cardinality" "presentnnv"
                        "presentv" "previous" "rank" "ratio_to_report" "rawtonhex" "ref" "reftohex" "regexp_instr" "regexp_replace"
                        "regexp_substr" "regr_slope" "regr_intercept" "regr_count" "regr_r2" "regr_avgx" "regr_avgy" "regr_sxx" "regr_syy"
                        "regr_sxy" "remainder" "row_number" "rowidtonchar" "scn_to_timestamp" "sessiontimezone" "stats_binomial_test"
                        "stats_crosstab" "stats_f_test" "stats_ks_test" "stats_mode" "stats_mw_test" "stats_one_way_anova" "stats_t_test_one"
                        "stats_t_test_paired" "stats_t_test_indep" "stats_t_test_indepu" "stats_wsr_test" "stddev_pop" "stddev_samp"
                        "sys_connect_by_path" "sys_context" "sys_dburigen" "sys_extract_utc" "sys_guid" "sys_typeid" "sys_xmlagg" "sys_xmlgen"
                        "systimestamp" "timestamp_to_scn" "to_binary_double" "to_binary_float" "to_clob" "to_dsinterval" "to_lob" "to_nchar"
                        "to_nclob" "to_timestamp" "to_timestamp_tz" "to_yminterval" "treat" "tz_offset" "unistr" "updatexml" "value" "var_pop"
                        "var_samp" "width_bucket" "xmlagg" "xmlcolattval" "xmlconcat" "xmlelement" "xmlforest" "xmlsequence" "xmltransform") t)
          "\\b"))
(defvar sqlplus-oracle-extra-warning-words-re
  (concat "\\b"
          (regexp-opt '("access_into_null" "case_not_found" "collection_is_null" "rowtype_mismatch"
                        "self_is_null" "subscript_beyond_count" "subscript_outside_limit" "sys_invalid_rowid") t)
          "\\b"))
(defvar sqlplus-oracle-extra-keywords-re
  (concat "\\b\\("
          "\\(at\\s-+local\\|at\\s-+time\\s-+zone\\|to\\s-+second\\|to\\s-+month\\|is\\s-+present\\|a\\s-+set\\)\\|"
          (regexp-opt '("case" "nan" "infinite" "equals_path" "empty" "likec" "like2" "like4" "member"
                        "regexp_like" "submultiset" "under_path" "mlslabel") t)
          "\\)\\b"))
(defvar sqlplus-oracle-extra-pseudocolumns-re
  (concat "\\b"
          (regexp-opt '("connect_by_iscycle" "connect_by_isleaf" "versions_starttime" "versions_startscn"
                        "versions_endtime" "versions_endscn" "versions_xid" "versions_operation" "object_id" "object_value" "ora_rowscn"
                        "xmldata") t)
          "\\b"))
(defvar sqlplus-oracle-plsql-extra-reserved-words-re
  (concat "\\b"
          (regexp-opt '("array" "at" "authid" "bulk" "char_base" "day" "do" "extends" "forall" "heap" "hour"
                        "interface" "isolation" "java" "limited" "minute" "mlslabel" "month" "natural" "naturaln" "nocopy" "number_base"
                        "ocirowid" "opaque" "operator" "organization" "pls_integer" "positive" "positiven" "range" "record" "release" "reverse"
                        "rowtype" "second" "separate" "space" "sql" "timezone_region" "timezone_abbr" "timezone_minute" "timezone_hour" "year"
                        "zone") t)
          "\\b"))
(defvar sqlplus-oracle-extra-types-re
  (concat "\\b"
          (regexp-opt '("nvarchar2" "binary_float" "binary_double" "timestamp" "interval" "interval_day" "urowid" "nchar" "clob" "nclob" "bfile") t)
          "\\b"))

(defvar sqlplus-commands-regexp-1 nil)
(defvar sqlplus-commands-regexp-23 nil)
(defvar sqlplus-system-variables-regexp-1 nil)
(defvar sqlplus-system-variables-regexp-23 nil)
(defvar sqlplus-v22-commands-font-lock-keywords-1 nil)
(defvar sqlplus-v22-commands-font-lock-keywords-23 nil)
(defvar font-lock-sqlplus-face nil)

(defvar sqlplus-output-buffer-keymap nil)
(make-variable-buffer-local 'sqlplus-output-buffer-keymap)

(defvar sqlplus-kill-function-inhibitor nil)

(defvar sqlplus-slip-separator-width 2
  "Only for classic table style.")

(defvar sqlplus-user-string-history nil)

(defvar sqlplus-object-types '( "CONSUMER GROUP" "SEQUENCE" "SCHEDULE" "PROCEDURE" "OPERATOR" "WINDOW"
                                "PACKAGE" "LIBRARY" "PROGRAM" "PACKAGE BODY" "JAVA RESOURCE" "XML SCHEMA"
                                "JOB CLASS" "TRIGGER" "TABLE" "SYNONYM" "VIEW" "FUNCTION" "WINDOW GROUP"
                                "JAVA CLASS" "INDEXTYPE" "INDEX" "TYPE" "EVALUATION CONTEXT" ))

(defvar sqlplus-end-of-source-sentinel "%%@@end-of-source-sentinel@@%%")

(defconst sqlplus-system-variables
  '("appi[nfo]" "array[size]" "auto[commit]" "autop[rint]" "autorecovery" "autot[race]" "blo[ckterminator]" "cmds[ep]"
    "colsep" "com[patibility]" "con[cat]" "copyc[ommit]" "copytypecheck" "def[ine]" "describe" "echo" "editf[ile]"
    "emb[edded]" "esc[ape]" "feed[back]" "flagger" "flu[sh]" "hea[ding]" "heads[ep]" "instance" "lin[esize]"
    "lobof[fset]" "logsource" "long" "longc[hunksize]" "mark[up]" "newp[age]" "null" "numf[ormat]" "num[width]"
    "pages[ize]" "pau[se]" "recsep" "recsepchar" "serverout[put]" "shift[inout]" "show[mode]" "sqlbl[anklines]"
    "sqlc[ase]" "sqlco[ntinue]" "sqln[umber]" "sqlpluscompat[ibility]" "sqlpre[fix]" "sqlp[rompt]" "sqlt[erminator]"
    "suf[fix]" "tab" "term[out]" "ti[me]" "timi[ng]" "trim[out]" "trims[pool]" "und[erline]" "ver[ify]" "wra[p]"))

(defconst sqlplus-commands
  '(("@[@]")
    (("/" "r[un]"))
    ("acc[ept]"
     (font-lock-type-face "num[ber]" "char" "date" "binary_float" "binary_double")
     (font-lock-keyword-face "for[mat]" "def[ault]" "[no]prompt" "hide"))
    ("a[ppend]")
    ("archive log"
     (font-lock-keyword-face "list" "stop" "start" "next" "all" "to"))
    ("attribute"
     (font-lock-keyword-face "ali[as]" "cle[ar]" "for[mat]" "like" "on" "off"))
    ("bre[ak]"
     (font-lock-keyword-face "on" "row" "report" "ski[p]" "page" "nodup[licates]" "dup[licates]"))
    ("bti[tle]"
     (font-lock-keyword-face "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("c[hange]")
    ("cl[ear]"
     (font-lock-keyword-face "bre[aks]" "buff[er]" "col[umns]" "comp[utes]" "scr[een]" "sql" "timi[ng]"))
    ("col[umn]"
     (font-lock-keyword-face "ali[as]" "cle[ar]" "entmap" "on" "off" "fold_a[fter]" "fold_b[efore]" "for[mat]" "hea[ding]"
                             "jus[tify]" "l[eft]" "c[enter]" "r[ight]" "like" "newl[ine]" "new_v[alue]" "nopri[nt]" "pri[nt]"
                             "nul[l]" "old_v[alue]" "wra[pped]" "wor[d_wrapped]" "tru[ncated]"))
    ("comp[ute]"
     (font-lock-keyword-face "lab[el]" "of" "on" "report" "row")
     (font-lock-builtin-face "avg" "cou[nt]" "min[imum]" "max[imum]" "num[ber]" "sum" "std" "var[iance]"))
    ("conn[ect]"
     (font-lock-keyword-face "as" "sysoper" "sysdba"))
    ("copy")
    ("def[ine]")
    ("del"
     (font-lock-keyword-face "last"))
    ("desc[ribe]")
    ("disc[onnect]")
    ("ed[it]")
    ("exec[ute]")
    (("exit" "quit")
     (font-lock-keyword-face "success" "failure" "warning" "commit" "rollback"))
    ("get"
     (font-lock-keyword-face "file" "lis[t]" "nol[ist]"))
    ("help")
    (("ho[st]" "!" "$"))
    ("i[nput]")
    ("l[ist]"
     (font-lock-keyword-face "last"))
    ("passw[ord]")
    ("pau[se]")
    ("pri[nt]")
    ("pro[mpt]")
    ("recover"
     (font-lock-keyword-face "begin" "end" "backup" "automatic" "from" "logfile" "test" "allow" "corruption" "continue" "default" "cancel"
                             "standby" "database" "until" "time" "change" "using" "controlfile" "tablespace" "datafile"
                             "consistent" "with" "[no]parallel" "managed" "disconnect" "session" "[no]timeout" "[no]delay" "next" "no" "expire"
                             "current" "through" "thread" "sequence" "all" "archivelog" "last" "switchover" "immediate" "[no]wait"
                             "finish" "skip"))
    ("rem[ark]")
    ("repf[ooter]"
     (font-lock-keyword-face "page" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("reph[eader]"
     (font-lock-keyword-face "page" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("sav[e]"
     (font-lock-keyword-face "file" "cre[ate]" "rep[lace]" "app[end]"))
    ("set"
     (font-lock-builtin-face sqlplus-system-variables)
     (font-lock-keyword-face "on" "off" "immediate" "trace[only]" "explain" "statistics" "native" "v7" "v8" "all" "linenum" "indent"
                             "entry" "intermediate" "full" "local" "head" "html" "body" "table" "entmap" "spool" "[pre]format"
                             "none" "[word_]wrapped" "each" "truncated" "[in]visible" "mixed" "lower" "upper"))
    ("sho[w]"
     (font-lock-keyword-face "all" "bti[tle]" "err[ors]" "function" "procedure" "package[ body]" "trigger" "view" "type[ body]"
                             "dimension" "java class" "lno" "parameters" "pno" "recyc[lebin]" "rel[ease]" "repf[ooter]" "reph[eader]"
                             "sga" "spoo[l]" "sqlcode" "tti[tle]" "user")
     (font-lock-builtin-face sqlplus-system-variables))
    ("shutdown"
     (font-lock-keyword-face "abort" "immediate" "normal" "transactional" "local"))
    ("spo[ol]"
     ("cre" "create" "rep" "replace" "app" "append" "off" "out"))
    ("sta[rt]")
    ("startup"
     (font-lock-keyword-face "force" "restrict" "pfile" "quiet" "mount" "open" "nomount" "read" "only" "write" "recover"))
    ("store"
     (font-lock-keyword-face "set" "cre[ate]" "rep[lace]" "app[end]"))
    ("timi[ng]"
     (font-lock-keyword-face "start" "show" "stop"))
    ("tti[tle]"
     (font-lock-keyword-face "tti[tle]" "on" "off")
     (font-lock-builtin-face "bold" "ce[nter]" "col" "format" "le[ft]" "r[ight]" "s[kip]" "tab"))
    ("undef[ine]")
    ("var[iable]"
     (font-lock-type-face "number" "[n]char" "byte" "[n]varchar2" "[n]clob" "refcursor" "binary_float" "binary_double"))
    ("whenever oserror"
     (font-lock-keyword-face "exit" "success" "failure" "commit" "rollback" "continue" "commit" "rollback" "none"))
    ("whenever sqlerror"
     (font-lock-keyword-face "exit" "success" "failure" "warning" "commit" "rollback" "continue" "none"))))

(defvar plsql-mode-map nil)

;;; ---

(defun sqlplus-initial-strings ()
  (append sqlplus-initial-strings
          (list 
           (concat "btitle left '" sqlplus-page-separator "'")
           (concat "repfooter left '" sqlplus-repfooter "'")
           (concat "set pagesize " (number-to-string sqlplus-pagesize)))))

(defun sqlplus--connect-string-parse (connect-string)
  (string-match "^\\([^@]*\\)?@?\\(.*\\)$" connect-string)
  (cons (match-string 1 connect-string) (match-string 2 connect-string)))

(defun sqlplus-connect-string-lessp (cs1 cs2)
  (let ((cs1-pair (sqlplus--connect-string-parse cs1))
        (cs2-pair (sqlplus--connect-string-parse cs1)))
    (or (string< (cdr cs1-pair) (cdr cs2-pair))
        (and (string= (cdr cs1-pair) (cdr cs2-pair))
             (string< (car cs1-pair) (car cs2-pair))))))

(defun sqlplus-divide-connect-strings ()
  (let* ((active-connect-strings
          (sort (delq nil (mapcar (lambda (buffer)
                                    (with-current-buffer buffer
                                      (when (and (eq major-mode 'sqlplus-mode)
                                                 sqlplus-connect-string)
                                        (let ((cs (car (refine-connect-string sqlplus-connect-string))))
                                          (when (and (get-buffer (sqlplus-get-process-buffer-name cs))
                                                     (get-process (sqlplus-get-process-name cs)))
                                            (downcase cs))))))
                                  (buffer-list)))
                'sqlplus-connect-string-lessp))
         (inactive-connect-strings
          (sort (delq nil (mapcar (lambda (pair)
                                    (unless (member (downcase (car pair)) active-connect-strings) (downcase (car pair))) )
                                  sqlplus-connect-strings-alist))
                'sqlplus-connect-string-lessp)))
    (setq active-connect-strings (remove-duplicates active-connect-strings :test 'equal))
    (setq inactive-connect-strings (remove-duplicates inactive-connect-strings :test 'equal))
    (cons active-connect-strings inactive-connect-strings)))

(defun sqlplus-connections-menu (menu)
  (condition-case err
      (let* ((connect-strings-pair (sqlplus-divide-connect-strings))
             (active-connect-strings (car connect-strings-pair))
             (inactive-connect-strings (cdr connect-strings-pair)))
        (append 
	 (list ["New connection..." sqlplus t])
	 (when (eq major-mode 'sqlplus-mode)
	   (list
	    "----"
	    ["Evaluate Statement" sqlplus-send-current sqlplus-connect-string]
	    ["Evaluate Statement (HTML)" sqlplus-send-current-html sqlplus-connect-string]
	    ["Evaluate Region" sqlplus-send-region (and (mark) sqlplus-connect-string)]))
	 (when orcl-mode
	   (list
	    "----"
	    ["Send Commit"              sqlplus-send-commit sqlplus-connect-string]
	    ["Send Rollback"            sqlplus-send-rollback sqlplus-connect-string]
	    ["Restart Connection"       sqlplus-restart-connection sqlplus-connect-string]
	    ["Show History"             sqlplus-show-history sqlplus-connect-string]
	    ["Get Source from DB"       sqlplus-get-source sqlplus-connect-string]
	    ["Interrupt Evaluation"     sqlplus-send-interrupt sqlplus-connect-string]
	    "----"
	    (list "Output"
		  ["Show window"             sqlplus-buffer-display-window t]
		  "----"
		  ["Redisplay"               sqlplus-buffer-redisplay-current t]
		  ["Previous"                sqlplus-buffer-prev-command t]
		  ["Next"                    sqlplus-buffer-next-command t]
		  "----"
		  ["Scroll Right"            sqlplus-buffer-scroll-right t]
		  ["Scroll Left"             sqlplus-buffer-scroll-left t]
		  ["Scroll Down"             sqlplus-buffer-scroll-down t]
		  ["Scroll Up"               sqlplus-buffer-scroll-up t]
		  "----"
		  ["Bottom"                  sqlplus-buffer-bottom t]
		  ["Top"                     sqlplus-buffer-top    t]
		  "----"
		  ["Erase"                   sqlplus-buffer-erase  t])
	    ))
	 (when inactive-connect-strings
	   (append
	    (list "----")
	    (list (append (list "Recent Connections")
			  (mapcar (lambda (connect-string)
				    (vector connect-string (list 'apply ''sqlplus
								 (list 'sqlplus-read-connect-string connect-string)) t)) inactive-connect-strings)))))
	 (when active-connect-strings
	   (append
	    (list "----")
	    (mapcar (lambda (connect-string)
		      (vector connect-string (list 'apply ''sqlplus
						   (list 'sqlplus-read-connect-string connect-string)) t)) active-connect-strings)))
	 ))
    (error (message (error-message-string err)))))

(defun sqlplus-send-commit ()
  "Send 'commit' command to SQL*Process."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-execute sqlplus-connect-string "commit;" nil nil))

(defun sqlplus-send-rollback ()
  "Send 'rollback' command to SQL*Process."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-execute sqlplus-connect-string "rollback;" nil nil))

(defun sqlplus-show-history ()
  "Show command history for current connection."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-verify-buffer sqlplus-connect-string)
  (switch-to-buffer (sqlplus-get-history-buffer sqlplus-connect-string)))

(defun sqlplus-restart-connection ()
  "Kill SQL*Plus process and start again."
  (interactive)
  (sqlplus-check-connection)
  (sqlplus-verify-buffer sqlplus-connect-string)
  (let ((connect-stringos sqlplus-connect-string))
    (unwind-protect
        (progn
          (setq sqlplus-kill-function-inhibitor t)
          (sqlplus-shutdown connect-stringos t))
      (setq sqlplus-kill-function-inhibitor nil))
    (sqlplus connect-stringos (car (refine-connect-string connect-stringos)))))

(define-skeleton plsql-begin
  "begin..end skeleton"
  "" ; interactor
  "begin" ?\n
  > _ ?\n
  "end;" >)

(define-skeleton plsql-loop
  "loop..end loop skeleton"
  "" ; interactor
  "loop" ?\n
  > _ ?\n
  "end loop;" >)

(define-skeleton plsql-if
  "if..end if skeleton"
  "" ; interactor
  "if " _ " then" ?\n
  > ?\n
  "end if;" >)

;;;  SQLPLUS-mode Keymap -

(unless orcl-mode-map
  (setq orcl-mode-map (make-sparse-keymap))
  (define-key orcl-mode-map "\C-c\C-o" 'sqlplus-buffer-display-window)
  (define-key orcl-mode-map "\C-c\C-l" 'sqlplus-buffer-redisplay-current)
  (define-key orcl-mode-map "\C-c\C-p" 'sqlplus-buffer-prev-command)
  (define-key orcl-mode-map [C-S-up] 'sqlplus-buffer-prev-command)
  (define-key orcl-mode-map "\C-c\C-n" 'sqlplus-buffer-next-command)
  (define-key orcl-mode-map [C-S-down] 'sqlplus-buffer-next-command)
  (define-key orcl-mode-map "\C-c\C-b" 'sqlplus-buffer-scroll-right)
  (define-key orcl-mode-map [C-S-left] 'sqlplus-buffer-scroll-right)
  (define-key orcl-mode-map "\C-c\C-f" 'sqlplus-buffer-scroll-left)
  (define-key orcl-mode-map [C-S-right] 'sqlplus-buffer-scroll-left)
  (define-key orcl-mode-map "\C-c\M-v" 'sqlplus-buffer-scroll-down)
  (define-key orcl-mode-map "\C-c\C-v" 'sqlplus-buffer-scroll-up)
  (define-key orcl-mode-map "\C-c>"    'sqlplus-buffer-bottom)
  (define-key orcl-mode-map "\C-c<"    'sqlplus-buffer-top)
  (define-key orcl-mode-map "\C-c\C-w" 'sqlplus-buffer-erase)
  (define-key orcl-mode-map "\C-c\C-m" 'sqlplus-send-commit)
  (define-key orcl-mode-map "\C-c\C-a" 'sqlplus-send-rollback)
  (define-key orcl-mode-map "\C-c\C-k" 'sqlplus-restart-connection)
  (define-key orcl-mode-map "\C-c\C-t" 'sqlplus-show-history)
  (define-key orcl-mode-map "\C-c\C-s" 'sqlplus-get-source)
  (define-key orcl-mode-map "\C-c\C-i" 'sqlplus-send-interrupt)
  (define-key orcl-mode-map [S-return] 'sqlplus-send-user-string)
  (define-key orcl-mode-map [tool-bar sqlplus-restart-connection]
    (list 'menu-item "Restart connection" 'sqlplus-restart-connection :image sqlplus-kill-image))
  (define-key orcl-mode-map [tool-bar sqlplus-cancel]
    (list 'menu-item "Cancel" 'sqlplus-send-interrupt :image sqlplus-cancel-image))
  (define-key orcl-mode-map [tool-bar sqlplus-rollback]
    (list 'menu-item "Rollback" 'sqlplus-send-rollback :image sqlplus-rollback-image))
  (define-key orcl-mode-map [tool-bar sqlplus-commit]
    (list 'menu-item "Commit" 'sqlplus-send-commit :image sqlplus-commit-image)))

(unless sqlplus-mode-map
  (setq sqlplus-mode-map (make-sparse-keymap))
  (define-key sqlplus-mode-map "\C-c\C-g" 'plsql-begin)
  (define-key sqlplus-mode-map "\C-c\C-q" 'plsql-loop)
  (define-key sqlplus-mode-map "\C-c\C-z" 'plsql-if)
  (define-key sqlplus-mode-map "\C-c\C-r" 'sqlplus-send-region)
  (define-key sqlplus-mode-map [C-return] 'sqlplus-send-current)
  (define-key sqlplus-mode-map "\C-c\C-e" 'sqlplus-send-current)
  (define-key sqlplus-mode-map "\C-c\C-j" 'sqlplus-send-current-html)
  (define-key sqlplus-mode-map [C-S-return] 'sqlplus-send-current-html))

(easy-menu-add-item nil nil sqlplus-connections-menu t)

(unless sqlplus-mode-syntax-table
  (setq sqlplus-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" sqlplus-mode-syntax-table) ; comment start
  (modify-syntax-entry ?* ". 23" sqlplus-mode-syntax-table)
  (modify-syntax-entry ?+ "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?. "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\" "."   sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\\ "."   sqlplus-mode-syntax-table)
  (modify-syntax-entry ?- ". 12b"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?\n "> b"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?= "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?% "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?< "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?> "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?& "w"    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?| "."    sqlplus-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    sqlplus-mode-syntax-table) ; _ is word char
  (modify-syntax-entry ?\' "\"" sqlplus-mode-syntax-table))

;;;  SQL*Plus mode

(defun connect-string-to-string ()
  (let ((txt (concat " " (or (car (refine-connect-string sqlplus-connect-string)) "disconnected"))))
    (if (>= emacs-major-version 22)
        (list :propertize txt 'face '((:foreground "blue")))
      (propertize txt 'face '((:foreground "blue"))))))

(defun sqlplus-font-lock (type-symbol limit)
  (when sqlplus-font-lock-regexps
    (let ((regexp (gethash type-symbol sqlplus-font-lock-regexps)))
      (when regexp
	(re-search-forward regexp limit t)))))

(defun sqlplus-find-begin-of-sqlplus-command ()
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp)) (save-excursion (end-of-line 0) (skip-chars-backward " \t") (equal (char-before) ?-)))
      (beginning-of-line 0))
    (point)))

(defun sqlplus-find-end-of-sqlplus-command ()
  (save-excursion
    (end-of-line)
    (while (progn (skip-chars-backward " \t") (and (not (eobp)) (equal (char-before) ?-)))
      (end-of-line 2))
    (point)))

(defun sqlplus-set-font-lock-emacs-structures-for-level (level mode-symbol)
  (let ((result (append sql-mode-oracle-font-lock-keywords
                        (default-value (cond ((equal level 3) 'sqlplus-font-lock-keywords-3)
                                             ((equal level 2) 'sqlplus-font-lock-keywords-2)
                                             ((equal level 1) 'sqlplus-font-lock-keywords-1)
                                             (t nil))))))
    (when (featurep 'plsql)
      (setq result (append (symbol-value 'plsql-oracle-font-lock-fix-re) result)))
    (setq result
          (append
           ;; Names for schemas, tables, synonyms, views, columns, sequences, packages, triggers and indexes
           (when (> level 2)
             (mapcar (lambda (pair)
                       (let ((type-symbol (car pair))
                             (face (cadr pair)))
                         (cons (eval `(lambda (limit) (sqlplus-font-lock ',type-symbol limit))) face)))
                     sqlplus-syntax-faces))
           ;; SQL*Plus
           (when (eq mode-symbol 'sqlplus-mode)
             (unless sqlplus-commands-regexp-1
               (flet ((first-form-fun (cmds) (mapcar (lambda (name) (car (sqlplus-full-forms name))) cmds))
                      (all-forms-fun (cmds) (mapcan 'sqlplus-full-forms cmds))
                      (sqlplus-commands-regexp-fun (form-fun cmds) (concat "^" (regexp-opt (funcall form-fun cmds) t) "\\b"))
                      (sqlplus-system-variables-fun (form-fun vars) (concat "\\b" (regexp-opt (funcall form-fun vars) t) "\\b")))
                 (flet ((sqlplus-v22-commands-font-lock-keywords-fun
                         (form-fun)
                         (delq nil
                               (mapcar
                                (lambda (command-info)
                                  (let* ((names (car command-info))
                                         (names-list (if (listp names) names (list names)))
                                         (sublists (cdr command-info)))
                                    (when sublists
                                      (append (list (sqlplus-commands-regexp-fun form-fun names-list))
                                              (mapcar (lambda (sublist)
                                                        (let ((face (car sublist))
                                                              (regexp (concat "\\b"
                                                                              (regexp-opt (mapcan (lambda (name) (sqlplus-full-forms name))
                                                                                                  (mapcan (lambda (elem)
                                                                                                            (if (symbolp elem)
                                                                                                                (copy-list (symbol-value elem))
                                                                                                              (list elem)))
                                                                                                          (cdr sublist)))
                                                                                          t)
                                                                              "\\b")))
                                                          (list regexp '(sqlplus-find-end-of-sqlplus-command) nil (list 1 face))))
                                                      sublists)
                                              (list '("\\(\\w+\\)" (sqlplus-find-end-of-sqlplus-command) nil (1 font-lock-sqlplus-face)))))))
                                sqlplus-commands))))
                   (let ((commands (mapcan
                                    (lambda (command-info) (let ((names (car command-info))) (if (listp names) (copy-list names) (list names))))
                                    sqlplus-commands)))
                     (setq sqlplus-commands-regexp-1 (sqlplus-commands-regexp-fun 'first-form-fun commands))
                     (setq sqlplus-commands-regexp-23 (sqlplus-commands-regexp-fun 'all-forms-fun commands))
                     (if (<= emacs-major-version 21)
                         (setq sqlplus-system-variables-regexp-1 (sqlplus-system-variables-fun 'first-form-fun sqlplus-system-variables)
                               sqlplus-system-variables-regexp-23 (sqlplus-system-variables-fun 'all-forms-fun sqlplus-system-variables))
                       (setq sqlplus-v22-commands-font-lock-keywords-1 (sqlplus-v22-commands-font-lock-keywords-fun 'first-form-fun)
                             sqlplus-v22-commands-font-lock-keywords-23 (sqlplus-v22-commands-font-lock-keywords-fun 'all-forms-fun)))))))
             (append (list
                      ;; Comments (REM command)
                      (cons "^\\(rem\\)\\b\\(.*?\\)$" '((1 font-lock-keyword-face nil nil) (2 font-lock-comment-face t nil)))
                      ;; Predefined SQL*Plus variables
                      (cons (concat "\\b"
                                    (regexp-opt '("_CONNECT_IDENTIFIER" "_DATE" "_EDITOR" "_O_VERSION" "_O_RELEASE" "_PRIVILEGE"
                                                  "_SQLPLUS_RELEASE" "_USER") t)
                                    "\\b")
                            'font-lock-builtin-face)
                      ;; SQL*Plus commands (+ shortcuts if level >= 2)
                      (cons
                       (concat (if (>= level 2) sqlplus-commands-regexp-23 sqlplus-commands-regexp-1) "\\|^\\(@@\\|@\\|!\\|/\\|\\$\\)" )
                       'font-lock-keyword-face))
                     (if (<= emacs-major-version 21)
                         ;; SQL*Plus system variables (+ shortcuts if level >= 2)
                         (list (cons (if (>= level 2) sqlplus-system-variables-regexp-23 sqlplus-system-variables-regexp-1) 'font-lock-builtin-face))
                       ;; ver. >= 22
                       (if (>= level 2) sqlplus-v22-commands-font-lock-keywords-23 sqlplus-v22-commands-font-lock-keywords-1))))
            ; (cons "\\b\\([a-zA-Z$_#0-9]+\\)\\b\\.\\(\\b[a-zA-Z$_#0-9]+\\b\\)" '((1 font-lock-type-face nil nil)(2 font-lock-variable-name-face nil nil)))
           (list
            ;; Extra Oracle syntax highlighting, not recognized by sql-mode or plsql-mode
            (cons sqlplus-oracle-extra-types-re 'font-lock-type-face)
            (cons sqlplus-oracle-extra-warning-words-re 'font-lock-warning-face)
            (cons sqlplus-oracle-extra-types-re 'font-lock-type-face)
            (cons sqlplus-oracle-extra-keywords-re 'font-lock-keyword-face)
            (cons sqlplus-oracle-plsql-extra-reserved-words-re 'font-lock-keyword-face)
            (if (string-match "XEmacs\\|Lucid" emacs-version)
                (cons sqlplus-oracle-extra-pseudocolumns-re 'font-lock-preprocessor-face)
              (cons sqlplus-oracle-extra-pseudocolumns-re 'font-lock-builtin-face))
            (if (string-match "XEmacs\\|Lucid" emacs-version)
                (cons sqlplus-oracle-extra-builtin-functions-re 'font-lock-preprocessor-face)
              (cons sqlplus-oracle-extra-builtin-functions-re 'font-lock-builtin-face))
            ;; SQL*Plus variable names, like '&name' or '&&name'
            (cons "\\(\\b&[&a-zA-Z$_#0-9]+\\b\\)" 'font-lock-variable-name-face))
           result
           ;; Function calls
           (when (>= level 2)
             (list (cons "\\b\\(\\([a-zA-Z$_#0-9]+\\b\\)\\.\\)?\\(\\b[a-zA-Z$_#0-9]+\\b\\)\\s-*("
                         '((2 font-lock-type-face nil t)
                           (3 font-lock-function-name-face nil nil)))))))
    result))

(defun sqlplus-mode nil
  "Mode for editing and executing SQL*Plus commands.  Entry into this mode runs the hook
'sqlplus-mode-hook'.

Use \\[sqlplus] to start the SQL*Plus interpreter.

Just position the cursor on or near the SQL*Plus statement you
wish to send and press '\\[sqlplus-send-current]' to run it and
display the results.

Mode Specific Bindings:

\\{sqlplus-mode-map}"
  (interactive)
  (setq major-mode 'sqlplus-mode)
  (setq mode-name "SQL*Plus")
  (use-local-map sqlplus-mode-map)
  (set-syntax-table sqlplus-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (orcl-mode 1)
  (setq sqlplus-font-lock-keywords-1 (sqlplus-set-font-lock-emacs-structures-for-level 1 major-mode))
  (setq sqlplus-font-lock-keywords-2 (sqlplus-set-font-lock-emacs-structures-for-level 2 major-mode))
  (setq sqlplus-font-lock-keywords-3 (sqlplus-set-font-lock-emacs-structures-for-level 3 major-mode))
  (when (featurep 'plsql)
    (set (make-local-variable 'indent-line-function) (lambda  () (interactive) (condition-case err (funcall (symbol-function 'plsql-indent)) (error (message "Error: %S" err)))))
    (set (make-local-variable 'indent-region-function) 'plsql-indent-region)
    (set (make-local-variable 'align-mode-rules-list) 'plsql-align-rules-list))
  (setq font-lock-defaults sqlplus-font-lock-defaults)
  (unless sqlplus-connect-string
    (let ((potential-connect-string (sqlplus-get-potential-connect-string (buffer-file-name))))
      (when (and potential-connect-string
                 (get-process (sqlplus-get-process-name potential-connect-string)))
        (setq sqlplus-connect-string potential-connect-string))))
  (set (make-local-variable 'font-lock-extend-after-change-region-function)
       (lambda (beg end old-len)
         (cons (save-excursion (goto-char beg) (sqlplus-find-begin-of-sqlplus-command))
               (save-excursion (goto-char end) (sqlplus-find-end-of-sqlplus-command)))))
  (unless font-lock-sqlplus-face
    (copy-face 'default 'font-lock-sqlplus-face)
    (setq font-lock-sqlplus-face 'font-lock-sqlplus-face))
  (turn-on-font-lock)
  (unless frame-background-mode
    (setq frame-background-mode (if (< (sqlplus-color-percentage (face-background 'default)) 50) 'dark 'light)))
  (run-hooks 'sqlplus-mode-hook))

(defun sqlplus-color-percentage (color)
  (truncate (* (/ (/ (reduce '+ (color-values color)) 3.0) 65535.0) 100.0)))

(defun sqlplus-get-potential-connect-string (file-path)
  (when file-path
    (let* ((file-name (file-name-nondirectory file-path))
           (extension (file-name-extension file-name))
           (case-fold-search t)
           (potential-connect-string
            (when (and extension
                       (string-match "^sqp$" extension)
                       (string-match "@" file-name))
              (car (refine-connect-string (file-name-sans-extension file-name))))))
      potential-connect-string)))

(defun sqlplus-check-connection ()
  (if orcl-mode
      (unless sqlplus-connect-string
        (let* ((potential-connect-string (sqlplus-get-potential-connect-string (buffer-file-name)))
               (connect-string (car (sqlplus-read-connect-string nil (or potential-connect-string (caar (sqlplus-divide-connect-strings)))))))
          (sqlplus connect-string (buffer-name))))
    (error "Current buffer is not determined to communicate with Oracle")))

;;;  Utilitities

(defun sqlplus-echo-in-buffer (buffer-name string &optional force-display hide-after-head)
  "Displays string in the named buffer, creating the buffer if needed.  If force-display is true, the buffer will appear
if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (with-current-buffer buffer

      (while (and (> (buffer-size) sqlplus-output-buffer-max-size)
                  (progn (goto-char (point-min))
                         (unless (eobp) (forward-char))
                         (re-search-forward  (concat "^" (regexp-quote sqlplus-output-separator)) nil t)))
        (delete-region 1 (- (point) (length sqlplus-output-separator))))

      (goto-char (point-max))
      (let ((start-point (point)))
        (insert string)
        (when hide-after-head
          (let ((from-pos (string-match "\n" string))
                (keymap (make-sparse-keymap))
                overlay)
            (when from-pos
              (setq overlay (make-overlay (+ start-point from-pos) (- (+ start-point (length string)) 2)))
              (when (or (not (consp buffer-invisibility-spec))
                        (not (assq 'hide-symbol buffer-invisibility-spec)))
                (add-to-invisibility-spec '(hide-symbol . t)))
              (overlay-put overlay 'invisible 'hide-symbol)
              (put-text-property start-point (- (+ start-point (length string)) 2) 'help-echo string)
              (put-text-property start-point (- (+ start-point (length string)) 2) 'mouse-face 'highlight)
              (put-text-property start-point (- (+ start-point (length string)) 2) 'keymap sqlplus-output-buffer-keymap)
              ))))
      (if force-display
          (set-window-point (get-buffer-window buffer-name) (point-max))))))

(defun sqlplus-verify-buffer (connect-string)
  (let ((output-buffer-name (sqlplus-get-output-buffer-name connect-string))
	(process-buffer-name (sqlplus-get-process-buffer-name connect-string)))
    (when (or (not (get-buffer output-buffer-name))
	      (not (get-buffer process-buffer-name)))
      (sqlplus-shutdown connect-string)
      (error "No SQL*Plus session!  Use 'M-x sqlplus' to start the SQL*Plus interpreter"))
    (unless (get-buffer-process process-buffer-name)
      (sqlplus-shutdown connect-string)
      (error "Buffer '%s' is not talking to anybody!" output-buffer-name)))
  t)

(defun sqlplus-get-context (connect-string &optional id)
  (let ((process-buffer (sqlplus-get-process-buffer-name connect-string)))
    (when process-buffer
      (with-current-buffer process-buffer
        (when id
          (while (and sqlplus-command-contexts
                      (not (equal (sqlplus-get-context-value (car sqlplus-command-contexts) :id) id)))
            (setq sqlplus-command-contexts (cdr sqlplus-command-contexts))))
        (car sqlplus-command-contexts)))))

(defun sqlplus-get-context-value (context var-symbol)
  (cdr (assq var-symbol context)))

(defun sqlplus-set-context-value (context var-symbol value)
  (let ((association (assq var-symbol context)))
    (if association
        (setcdr association value)
      (setcdr context (cons (cons var-symbol value) (cdr context))))
    context))

(defun sqlplus-mark-current ()
  "Marks the current SQL for sending to the SQL*Plus process.  Marks are placed around a region defined by empty lines."
  (let (begin end)
    (save-excursion
      (end-of-line)
      (re-search-backward "\\`\\|\n[\r\t ]*\n[^ \t]" nil t)
      (skip-syntax-forward "-")
      (setq begin (point)))
    (save-excursion
      (beginning-of-line)
      (re-search-forward "\n[\r\t ]*\n[^ \t]\\|\\'" nil t)
      (unless (zerop (length (match-string 0)))
        (backward-char 1))
      (skip-syntax-backward "-")
      (setq end (point)))
    (cons begin end)))

;;;  Transmission Commands

(defun sqlplus-send-current (arg &optional html)
  "Send the current SQL command(s) to the SQL*Plus process.  With argument, show results in raw form."
  (interactive "P")
  (sqlplus-check-connection)
  (when (buffer-file-name)
    (condition-case err
	(save-buffer)
      (error (error-message-string err))))
  (let ((region (sqlplus-mark-current)))
    (setq sqlplus-region-beginning-pos (car region)
          sqlplus-region-end-pos (cdr region)))
  (sqlplus-send-region arg sqlplus-region-beginning-pos sqlplus-region-end-pos nil html))

(defun sqlplus-send-current-html (arg)
  (interactive "P")
  (sqlplus-send-current arg t))


;;;  SQLPLUS-Output Buffer Operations -

(defun sqlplus--show-buffer (connect-string fcn args)
  (let* ((output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
    (sqlplus-verify-buffer connect-string)
    (if sqlplus-suppress-show-output-buffer
        (with-current-buffer (get-buffer output-buffer-name)
          (if fcn (condition-case err (apply fcn args) (error (message (error-message-string err))))))
      (if (not (eq (window-buffer (selected-window)) (get-buffer output-buffer-name)))
          (switch-to-buffer-other-window output-buffer-name))
      (if fcn (condition-case err (apply fcn args) (error (message (error-message-string err))))))))

(defun sqlplus-show-buffer (&optional connect-string fcn &rest args)
  "Makes the SQL*Plus output buffer visible in the other window."
  (interactive)
  (setq connect-string (or connect-string sqlplus-connect-string))
  (unless connect-string
    (error "Current buffer is disconnected!"))
  (let ((output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
    (if (and output-buffer-name
             (eq (current-buffer) (get-buffer output-buffer-name)))
        (sqlplus--show-buffer connect-string fcn args)
      (save-excursion
        (save-selected-window
          (sqlplus--show-buffer connect-string fcn args))))))

(fset 'sqlplus-buffer-display-window 'sqlplus-show-buffer)

(defun sqlplus-buffer-scroll-up (&optional connect-string)
  "Scroll-up in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'scroll-up))

(defun sqlplus-buffer-scroll-down (&optional connect-string)
  "Scroll-down in the SQL*Plus output buffer window."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'scroll-down))

(defun sqlplus-scroll-left (num)
  (call-interactively 'scroll-left))

(defun sqlplus-scroll-right (num)
  (call-interactively 'scroll-right))

(defun sqlplus-buffer-scroll-left (num &optional connect-string)
  "Scroll-left in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-scroll-left (* num (/ (window-width) 2))))

(defun sqlplus-buffer-scroll-right (num &optional connect-string)
  "Scroll-right in the SQL*Plus output buffer window."
  (interactive "p")
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-scroll-right (* num (/ (window-width) 2))))

(defun sqlplus-buffer-mark-current (&optional connect-string)
  "Mark the current position in the SQL*Plus output window."
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-buffer-make-mark))

(defun sqlplus-buffer-make-mark (&optional connect-string)
  "Set the sqlplus-buffer-marker."
  (setq sqlplus-buffer-mark (copy-marker (point))))

(defun sqlplus-buffer-redisplay-current (&optional connect-string)
  "Go to the current sqlplus-buffer-mark."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-goto-mark))

(defun sqlplus-goto-mark ()
  (goto-char sqlplus-buffer-mark)
  (recenter 0))

(defun sqlplus-buffer-top (&optional connect-string)
  "Goto the top of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-beginning-of-buffer))

(defun sqlplus-beginning-of-buffer nil (goto-char (point-min)))

(defun sqlplus-buffer-bottom (&optional connect-string)
  "Goto the bottom of the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-end-of-buffer))

(defun sqlplus-end-of-buffer nil (goto-char (point-max)) (unless sqlplus-suppress-show-output-buffer (recenter -1)))

(defun sqlplus-buffer-erase (&optional connect-string)
  "Clear the SQL output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'erase-buffer))

(defun sqlplus-buffer-next-command (&optional connect-string)
  "Search for the next command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-next-command))

(defun sqlplus-next-command nil
  "Search for the next command in the SQL*Plus output buffer."
  (cond ((re-search-forward  (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
	 (forward-line 2)
	 (recenter 0))
	(t (beep) (message "No more commands."))))

(defun sqlplus-buffer-prev-command (&optional connect-string)
  "Search for the previous command in the SQL*Plus output buffer."
  (interactive)
  (sqlplus-show-buffer (or connect-string sqlplus-connect-string) 'sqlplus-previous-command))

(defun sqlplus-previous-command nil
  "Search for the previous command in the SQL*Plus output buffer."
  (let ((start (point)))
    (re-search-backward (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
    (cond ((re-search-backward (concat "^" (regexp-quote sqlplus-output-separator)) nil t)
	   (forward-line 2)
	   (recenter 0))
	  (t
	   (message "No more commands.") (beep)
	   (goto-char start)))))

(defun sqlplus-send-interrupt nil
  "Send an interrupt the the SQL*Plus interpreter process."
  (interactive)
  (sqlplus-check-connection)
  (let ((connect-string sqlplus-connect-string))
    (sqlplus-verify-buffer connect-string)
    (interrupt-process (get-process (sqlplus-get-process-name connect-string)))))


;;;  SQL Interpreter

(defun refine-connect-string (connect-string &optional no-slash)
  "Z connect stringa do SQL*Plusa wycina haslo, tj. np. 'ponaglenia/x@SID' -> ('ponaglenia@SID' . 'x')."
  (let (result passwd)
    (when connect-string
      (setq result
	    (if (string-match "\\(\\`[^@/]*?\\)/\\([^/@:]*\\)\\(.*?\\'\\)" connect-string)
                (progn
                  (setq passwd (match-string 2 connect-string))
                  (concat (match-string 1 connect-string) (match-string 3 connect-string)))
	      connect-string))
      (when no-slash
	(while (string-match "/" result)
	  (setq result (replace-match "!" nil t result)))))
    (cons result passwd)))

(defun sqlplus-get-output-buffer-name (connect-string)
  (concat "*" (car (refine-connect-string connect-string)) "*"))

(defun sqlplus-get-input-buffer-name (connect-string)
  (concat (car (refine-connect-string connect-string)) ".sqp"))

(defun sqlplus-get-history-buffer-name (connect-string)
  (concat " " (car (refine-connect-string connect-string)) "-hist"))

(defun sqlplus-get-process-buffer-name (connect-string)
  (concat " " (car (refine-connect-string connect-string))))

(defun sqlplus-get-process-name (connect-string)
  (car (refine-connect-string connect-string)))

(defun sqlplus-read-connect-string (&optional connect-string default-connect-string)
  (unless default-connect-string
    (let ((inactive-connect-strings (cdr (sqlplus-divide-connect-strings))))
      (setq default-connect-string (some (lambda (pair) (when (member (car pair) inactive-connect-strings) (car pair))) sqlplus-connect-strings-alist))))
  (let* ((cs (downcase (or connect-string (read-string (format "Connect string%s: " (if default-connect-string (format " [default %s]" default-connect-string) ""))
                                             nil 'sqlplus-connect-string-history default-connect-string))))
         (pair (refine-connect-string cs))
         (refined-cs (car pair))
         (password (cdr pair))
         (was-password password)
         (association (assoc refined-cs sqlplus-connect-strings-alist)))
    (unless (or password current-prefix-arg)
      (setq password (cdr association)))
    (unless password
      (setq password (read-passwd (format "Password for %s: " cs))))
    (unless was-password
      (if (string-match "@" cs)
          (setq cs (replace-match (concat "/" password "@") t t cs))
        (setq cs (concat cs "/" password))))
    (list cs refined-cs)))

(defun sqlplus (connect-string &optional input-buffer-name dont-show-output-buffer)
  "Tworzy proces SQL*Plus z podanym CONNECT-STRINGiem jako parametr, otwierajac (lub tworzac) jednoczesnie sprzezony z
nim bufor wejsciowy INPUT-BUFFER-NAME (jesli nil, to nie tworzy takiego bufora)."
  (interactive (sqlplus-read-connect-string))
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|--+ *")
  (set (make-local-variable 'comment-multi-line) t)
  (when sqlplus-session-cache-dir
    (condition-case err
        (unless (file-directory-p sqlplus-session-cache-dir)
          (make-directory sqlplus-session-cache-dir t))
      (error (message (error-message-string err)))))
  (let* ((was-input-buffer (and input-buffer-name (get-buffer input-buffer-name)))
         (input-buffer (or was-input-buffer
                           (when input-buffer-name
                             (if sqlplus-session-cache-dir
                                 (let ((buf (find-file-noselect
                                             (concat
                                              (file-name-as-directory sqlplus-session-cache-dir)
                                              (car (refine-connect-string connect-string t))
                                              ".sqp"))))
                                   (condition-case nil
                                       (with-current-buffer buf
                                         (rename-buffer (sqlplus-get-input-buffer-name connect-string)))
                                     (error nil))
                                   buf)
                               (get-buffer-create input-buffer-name)))))
         (output-buffer (get-buffer-create (sqlplus-get-output-buffer-name connect-string)))
	 (process-name (sqlplus-get-process-name connect-string))
	 (process-buffer-name (sqlplus-get-process-buffer-name connect-string))
         (was-process (get-process process-name))
         created
         (process (or was-process
                      (let (proc)
			(with-current-buffer output-buffer
                          (erase-buffer)
                          (setq sqlplus-font-lock-regexps (make-hash-table))
			  (setq truncate-lines t)
			  (setq created t)
			  (setq proc (start-process process-name process-buffer-name sqlplus-command connect-string))
                          (let* ((refined-cs (refine-connect-string connect-string)))
                            (setq sqlplus-connect-strings-alist (delete* (car refined-cs) sqlplus-connect-strings-alist :test 'string= :key 'car))
                            (push refined-cs sqlplus-connect-strings-alist))
                          (sqlplus-get-history-buffer connect-string)
                          (set-process-sentinel proc (lambda (process event)
                                                       (let ((proc-buffer (process-buffer process))
                                                             (output-buffer (get-buffer (sqlplus-get-output-buffer-name (process-name process))))
                                                             err-msg
                                                             (exited-abnormally (string-match "\\`exited abnormally with code" event)))
                                                         (when output-buffer
                                                           (with-current-buffer output-buffer
                                                             (goto-char (point-max))
                                                             (insert (format "\n%s" event))
                                                             (when exited-abnormally
                                                               (setq sqlplus-connect-strings-alist
                                                                     (delete* (car (refine-connect-string sqlplus-connect-string))
                                                                              sqlplus-connect-strings-alist :test 'string= :key 'car))
                                                               (when proc-buffer
                                                                 (with-current-buffer proc-buffer
                                                                   (save-excursion
                                                                     (goto-char (point-min))
                                                                     (when (re-search-forward "^ORA-[0-9]+.*$" nil t)
                                                                       (setq err-msg (match-string 0))))
                                                                   (erase-buffer)))
                                                               (when err-msg
                                                                 (insert (concat "\n" err-msg))))))
                                                         )))
                          (process-kill-without-query proc (not sqlplus-kill-processes-without-query-on-exit-flag))
			  (set-process-filter proc 'sqlplus-process-filter))
			(with-current-buffer (get-buffer process-buffer-name)
			  (setq sqlplus-process-p connect-string))
			proc
			))))
    (with-current-buffer output-buffer
      (orcl-mode 1)
      (set (make-local-variable 'line-move-ignore-invisible) t)
      (setq sqlplus-output-buffer-keymap (make-sparse-keymap))
      (define-key sqlplus-output-buffer-keymap "\C-m" (lambda () (interactive) (sqlplus-output-buffer-hide-show)))
      (define-key sqlplus-output-buffer-keymap [S-mouse-2] (lambda (event) (interactive "@e") (sqlplus-output-buffer-hide-show)))
      (local-set-key [S-return] 'sqlplus-send-user-string)
      (setq sqlplus-connect-string connect-string))
    (when input-buffer
      (with-current-buffer input-buffer
        (setq sqlplus-connect-string connect-string)))
    (when (and input-buffer (not was-input-buffer))
      (with-current-buffer input-buffer
        (unless (eq major-mode 'sqlplus-mode)
          (sqlplus-mode)))
      (set-window-buffer (sqlplus-get-workbench-window) input-buffer))
    (when input-buffer
      (let (regexp-map)
        (with-current-buffer output-buffer
          (setq regexp-map sqlplus-font-lock-regexps))
        (with-current-buffer input-buffer
          (sqlplus-set-font-lock-data regexp-map))))
    (when created
      (sqlplus-execute connect-string nil nil (sqlplus-initial-strings) 'no-echo)
      (let ((plsql-font-lock-level (sqlplus-font-lock-value-in-major-mode font-lock-maximum-decoration 'plsql-mode))
            (sqlplus-font-lock-level (sqlplus-font-lock-value-in-major-mode font-lock-maximum-decoration 'sqlplus-mode)))
        (when (or (equal plsql-font-lock-level t) (equal sqlplus-font-lock-level t)
                  (and (numberp plsql-font-lock-level) (>= plsql-font-lock-level 2))
                  (and (numberp sqlplus-font-lock-level) (>= sqlplus-font-lock-level 2)))
          (sqlplus-hidden-select connect-string 
                                 (concat "select distinct column_name, 'COLUMN' from user_tab_columns "
                                         "union "
                                         "select username, 'SCHEMA' from all_users "
                                         "union "
                                         "select object_name, object_type from user_objects "
                                         "where object_type in ('VIEW', 'SEQUENCE', 'PACKAGE', 'TRIGGER', 'TABLE', 'SYNONYM', 'INDEX');")
                                 'sqlplus-my-handler))))
    (when input-buffer
      (save-selected-window
        (when (equal (selected-window) (sqlplus-get-side-window))
          (select-window (sqlplus-get-workbench-window)))
        (switch-to-buffer input-buffer)))
    (unless dont-show-output-buffer
      (sqlplus-show-buffer connect-string))
    (when (and (not was-process) input-buffer)
      (with-current-buffer input-buffer
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (concat "^" sqlplus-init-sequence-start-regexp "\\s-*\n\\(\\(.\\|\n\\)*?\\)\n" sqlplus-init-sequence-end-regexp) nil t)
            (when (match-string 1)
              (sqlplus-send-region nil (match-beginning 1) (match-end 1) t)))
          )))))

(defun sqlplus-output-buffer-hide-show ()
  (if (and (consp buffer-invisibility-spec)
           (assq 'hide-symbol buffer-invisibility-spec))
      (remove-from-invisibility-spec '(hide-symbol . t))
    (add-to-invisibility-spec '(hide-symbol . t)))
  (let ((overlay (car (overlays-at (point)))))
    (when overlay
      (goto-char (overlay-start overlay))
      (beginning-of-line)))
  (recenter 0))

(defun sqlplus-font-lock-value-in-major-mode (alist mode-symbol)
  (if (consp alist)
      (cdr (or (assq mode-symbol alist) (assq t alist)))
    alist))

(defun sqlplus-get-history-buffer (connect-string)
  (let* ((history-buffer-name (sqlplus-get-history-buffer-name connect-string))
         (history-buffer (get-buffer history-buffer-name)))
    (unless history-buffer
      (setq history-buffer (get-buffer-create history-buffer-name))
      (with-current-buffer history-buffer
        (setq sqlplus-cs connect-string)
        (add-hook 'kill-buffer-hook 'sqlplus-history-buffer-kill-function nil t)))
    history-buffer))

(defun sqlplus-history-buffer-kill-function ()
  (when sqlplus-history-dir
    (condition-case err
        (progn
          (unless (file-directory-p sqlplus-history-dir)
            (make-directory sqlplus-history-dir t))
          (append-to-file 1 (buffer-size) (concat (file-name-as-directory sqlplus-history-dir) (car (refine-connect-string sqlplus-cs t)) "-hist.txt")))
      (error (message (error-message-string err))))))

(defun sqlplus-shutdown (connect-string &optional dont-kill-input-buffer)
  "Kill input, output and process buffer for specified CONNECT-STRING."
  (let ((input-buffers (delq nil (mapcar (lambda (buffer) (with-current-buffer buffer
                                                            (when (and (eq major-mode 'sqlplus-mode)
                                                                       (equal (car (refine-connect-string sqlplus-connect-string))
                                                                              (car (refine-connect-string connect-string))))
                                                              buffer))) (buffer-list))))
	(output-buffer (get-buffer (sqlplus-get-output-buffer-name connect-string)))
        (history-buffer (get-buffer (sqlplus-get-history-buffer-name connect-string)))
	(process-buffer (get-buffer (sqlplus-get-process-buffer-name connect-string))))
    (when history-buffer
      (kill-buffer history-buffer))
    (when (and process-buffer
	       (with-current-buffer process-buffer sqlplus-process-p))
      (when (get-process (sqlplus-get-process-name connect-string))
        (delete-process (sqlplus-get-process-name connect-string)))
      (kill-buffer process-buffer))
    (when (and output-buffer
	       (with-current-buffer output-buffer sqlplus-connect-string))
      (when (buffer-file-name output-buffer)
	(with-current-buffer output-buffer
	  (save-buffer)))
      (kill-buffer output-buffer))
    (dolist (input-buffer input-buffers)
      (when (buffer-file-name input-buffer)
        (with-current-buffer input-buffer
          (save-buffer)))
      (unless dont-kill-input-buffer
        (kill-buffer input-buffer)))))

(defun sqlplus-process-command-output (context connect-string begin end interrupted)
  (let* ((output-buffer (sqlplus-get-output-buffer-name connect-string))
         (process-buffer (sqlplus-get-process-buffer-name connect-string))
         str
	 error-list show-errors-p
	 slips-count
         (result-function (sqlplus-get-context-value context :result-table-function))
         (last-compiled-file-path (sqlplus-get-context-value context :last-compiled-file-path))
         (compilation-expected (sqlplus-get-context-value context :compilation-expected))
         (columns-count (sqlplus-get-context-value context :columns-count))
	 table-data)
    (setq slips-count columns-count)
    (narrow-to-region begin end)
    (goto-char (point-min))
    (while (re-search-forward (concat "\n+" (regexp-quote sqlplus-page-separator) "\n") nil t)
       (replace-match "\n"))
    (goto-char (point-min))
    (setq str (buffer-string))

    (unwind-protect
	(progn
	  ;; compilation errors?
	  (goto-char (point-min))
	  (skip-chars-forward "\n\t ")
	  (when (and ;;(not (equal (point) (point-max)))
		     plsql-auto-parse-errors-flag
		     last-compiled-file-path
		     (re-search-forward "^\\(LINE/COL\\|\\(SP2\\|CPY\\|ORA\\)-[0-9]\\{4,5\\}:\\|No errors\\|Nie ma b..d.w\\|Keine Fehler\\|No hay errores\\|Identificateur erron\\|Nessun errore\\|N..o h.. erros\\)" nil t))
	    (goto-char (point-min))
	    (setq error-list (plsql-parse-errors last-compiled-file-path))
	    (setq show-errors-p compilation-expected))
	  ;; query result?
	  (goto-char (point-min))
	  (when (and sqlplus-format-output-tables-flag
		     (not compilation-expected)
		     (not show-errors-p)
		     (not (re-search-forward "^LINE/COL\\>" nil t)))
	    (setq table-data (save-excursion (sqlplus-parse-output-table interrupted)))))
      (widen))
    (when output-buffer
      (with-current-buffer output-buffer
	(save-excursion
	  (goto-char (point-max))
	  (cond (show-errors-p
                 (while (string-match (concat "^" (regexp-quote sqlplus-repfooter) "\n") str)
                   (setq str (replace-match "" nil t str)))
                 (insert str)
		 (plsql-display-errors (file-name-directory last-compiled-file-path) error-list)
		 (let* ((plsql-buf (get-file-buffer last-compiled-file-path))
			(win (when plsql-buf (car (get-buffer-window-list plsql-buf)))))
		   (when win
		     (select-window win))))
		((and table-data
		      (car table-data))
                 (if result-function
                     (funcall result-function connect-string table-data)
                   (sqlplus-draw-table table-data slips-count)))
		(t
                 (while (string-match (concat "^" (regexp-quote sqlplus-repfooter) "\n") str)
                   (setq str (replace-match "" nil t str)))
		 (insert str)))
          (when interrupted (insert ". . .\n")))))))

(defun sqlplus-result-online (connect-string context string last-chunk)
  (let ((output-buffer (sqlplus-get-output-buffer-name connect-string)))
    (when output-buffer
      (with-current-buffer output-buffer
        (save-excursion
          (goto-char (point-max))
          (insert string))))))

(defun sqlplus-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (let* ((prompt-regexp (concat "^" (regexp-quote sqlplus-prompt-prefix) "\\([0-9]+\\)" (regexp-quote sqlplus-prompt-suffix)))
           (prompt-safe-len (+ (max (+ (length sqlplus-prompt-prefix) (length sqlplus-prompt-suffix)) (length sqlplus-page-separator)) 10))
           current-context-id finish
           (connect-string sqlplus-process-p)
           (chunk-begin-pos (make-marker))
           (chunk-end-pos (make-marker))
           (prompt-found (make-marker))
           (insert-pos (point)))
      (set-marker chunk-begin-pos (max 1 (- (point) prompt-safe-len)))
      (goto-char (point-max))
      (insert string)
      (untabify insert-pos (point-max))
      ;; fan animation
      (let* ((context (sqlplus-get-context connect-string current-context-id))
             (current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name)))
        (when current-command-input-buffer-name
          (let ((input-buffer (get-buffer current-command-input-buffer-name)))
            (when input-buffer
              (with-current-buffer input-buffer
                (cond ((equal sqlplus-fan " |") (setq sqlplus-fan " /"))
                      ((equal sqlplus-fan " /") (setq sqlplus-fan " -"))
                      ((equal sqlplus-fan " -") (setq sqlplus-fan " \\"))
                      ((equal sqlplus-fan " \\") (setq sqlplus-fan " |")))
                (put-text-property 0 (length sqlplus-fan) 'face '((foreground-color . "red")) sqlplus-fan)
                (force-mode-line-update))))))
      (unwind-protect
          (while (not finish)
            (let* ((context (sqlplus-get-context connect-string current-context-id))
                   (dont-parse-result (sqlplus-get-context-value context :dont-parse-result))
                   (current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name))
                   (result-function (sqlplus-get-context-value context :result-function))
                   (skip-to-the-end-of-command (sqlplus-get-context-value context :skip-to-the-end-of-command)))
              (set-marker prompt-found nil)
              (save-excursion
                (goto-char chunk-begin-pos)
                (set-marker chunk-end-pos
                            (if (or (re-search-forward prompt-regexp nil t)
                                    (re-search-forward "^SQL> " nil t))
                                (progn
                                  (set-marker prompt-found (match-end 0))
                                  (when (match-string 1)
                                    (setq current-context-id (string-to-number (match-string 1))))
                                  (match-beginning 0))
                              (max chunk-begin-pos (- (point-max) prompt-safe-len)))))
              (cond ((and (equal chunk-begin-pos chunk-end-pos) ; at the end of command
                          (marker-position prompt-found))
                     (when current-command-input-buffer-name
                       (let ((input-buffer (get-buffer current-command-input-buffer-name)))
                         (when input-buffer
                           (with-current-buffer input-buffer
                             (set-text-properties 0 (length sqlplus-fan) nil sqlplus-fan)
                             (force-mode-line-update)))))
                     (delete-region 1 prompt-found)
                     (sqlplus-set-context-value context :skip-to-the-end-of-command nil)
                     (set-marker chunk-begin-pos 1))
                    ((equal chunk-begin-pos chunk-end-pos) ; input string processed
                     (setq finish t))
                    (dont-parse-result
                     (funcall (or result-function 'sqlplus-result-online)
                              connect-string
                              context
                              (buffer-substring chunk-begin-pos chunk-end-pos)
                              (marker-position prompt-found))
                     (set-marker chunk-begin-pos chunk-end-pos))
                    (t
                     (save-excursion
                       (set-marker chunk-begin-pos (max 1 (- chunk-begin-pos 4010)))
                       (goto-char chunk-begin-pos)
                       (let ((page-separator-regexp (concat "^" (regexp-quote sqlplus-page-separator))))
                         (if skip-to-the-end-of-command
                             (set-marker chunk-begin-pos chunk-end-pos)
                           (let ((page-separator-found (save-excursion (let ((pos (re-search-forward (concat page-separator-regexp "[^-]*\\(^-\\|^<th\\b\\)") nil t)))
                                                                         (when (and pos (or (not (marker-position prompt-found))
                                                                                            (< pos prompt-found)))
                                                                           (match-beginning 0))))))
                             (if (or (marker-position prompt-found) page-separator-found)
                                 (progn
                                   (goto-char (or page-separator-found chunk-end-pos))
                                   (let ((marker (point-marker))
                                         (cur-msg (or (current-message) "")))
                                     (when page-separator-found
                                       (interrupt-process))
                                     (sqlplus-set-context-value context :skip-to-the-end-of-command page-separator-found)
                                     (let ((end-pos (point)))
                                       (when page-separator-found
                                         (save-excursion
                                           (re-search-backward "[^ \t\n]\n" nil t)
                                           (setq end-pos (match-end 0))))
                                       (if result-function
                                           (save-excursion (funcall result-function context connect-string 1 end-pos page-separator-found))
                                         (with-temp-message "Formatting output..."
                                           (save-excursion (sqlplus-process-command-output context connect-string 1 end-pos page-separator-found)))
                                         (message "%s" cur-msg))
                                       (when page-separator-found
                                         (delete-region 1 (+ page-separator-found (length sqlplus-page-separator)))
                                         (set-marker chunk-end-pos 1))
                                       (set-marker chunk-begin-pos chunk-end-pos)
                                       (set-marker marker nil))))
                               (set-marker chunk-begin-pos chunk-end-pos))))))))))
        (set-marker chunk-begin-pos nil)
        (set-marker chunk-end-pos nil)
        (set-marker prompt-found nil)))))

(defadvice switch-to-buffer (around switch-to-buffer-around-advice (buffer-or-name &optional norecord))
  ad-do-it
  (when (and sqlplus-connect-string
	     (eq major-mode 'sqlplus-mode))
    (let ((side-window (sqlplus-get-side-window))
          (output-buffer (get-buffer (sqlplus-get-output-buffer-name sqlplus-connect-string))))
      (when (and side-window
                 (not (eq (window-buffer) output-buffer)))
        (save-selected-window
          (switch-to-buffer-other-window output-buffer))))))
(ad-activate 'switch-to-buffer)

(defun sqlplus-kill-function ()
  (unless sqlplus-kill-function-inhibitor
    ;; shutdown connection if it is SQL*Plus output buffer or SQL*Plus process buffer
    (if (or (and sqlplus-connect-string (equal (buffer-name) (sqlplus-get-output-buffer-name sqlplus-connect-string)))
            sqlplus-process-p)
        (sqlplus--enqueue-task 'sqlplus-shutdown (or sqlplus-connect-string sqlplus-process-p))
      ;; input buffer or another buffer connected to SQL*Plus - possibly shutdown
      (when sqlplus-connect-string
        (let ((counter 0)
              (scs sqlplus-connect-string))
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (equal sqlplus-connect-string scs) (incf counter))))
          (when (<= counter 2)
            (let* ((process (get-process (sqlplus-get-process-name sqlplus-connect-string))))
              (when (or (not process)
                        (memq (process-status process) '(exit signal))
                        (y-or-n-p (format "Kill SQL*Plus process %s " (car (refine-connect-string sqlplus-connect-string)))))
                (sqlplus--enqueue-task 'sqlplus-shutdown sqlplus-connect-string)))))))))

(defun sqlplus-emacs-kill-function ()
  ;; save and kill all sqlplus-mode buffers
  (let (buffers-to-kill)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (and sqlplus-connect-string
		   (eq major-mode 'sqlplus-mode))
	  (when (buffer-file-name)
	    (save-buffer))
	  (push buffer buffers-to-kill))))
    (setq sqlplus-kill-function-inhibitor t)
    (condition-case nil
	(unwind-protect
	    (dolist (buffer buffers-to-kill)
	      (kill-buffer buffer))
	  (setq sqlplus-kill-function-inhibitor nil))
      (error nil))
    t))

(push 'sqlplus-emacs-kill-function kill-emacs-query-functions)

(add-hook 'kill-buffer-hook 'sqlplus-kill-function)

;; kill all history buffers so that they can save themselves
(add-hook 'kill-emacs-hook (lambda ()
                             (dolist (buf (copy-list (buffer-list)))
                               (when (and (string-match "@.*-hist" (buffer-name buf))
                                          (with-current-buffer buf sqlplus-cs))
                                 (kill-buffer buf)))))

(defun sqlplus-find-output-table (interrupted)
  "Search for table in last SQL*Plus command result, and return
list (BEGIN END MSG) for first and last table char, or nil if
table is not found."
  (let (begin end)
    (goto-char (point-min))
    (when (re-search-forward "^[^\n]+\n\\( \\)?-" nil t)
      (let (msg
	    (indent (when (match-string 1) -1)))
	(forward-line -1)
	;; (untabify (point) (buffer-size))
	(setq begin (point))
	(when indent
	  (indent-rigidly begin (point-max) indent)
	  (goto-char begin))
	(if indent
	    (progn
	      (goto-char (point-max))
	      (skip-chars-backward "\n\t ")
	      (setq end (point))
	      (goto-char (point-max)))
	  (or (re-search-forward (concat "^" (regexp-quote sqlplus-repfooter) "\n[\n\t ]*") nil t)
              (when interrupted (re-search-forward "\\'" nil t)))
	  (setq end (match-beginning 0))
	  (setq msg (buffer-substring (match-end 0) (point-max))))
	(list begin end msg)))))

(defstruct col-desc
  id   ; from 0
  name ; column name
  start-pos ; char column number
  end-pos ; char column number
  max-width ; max. column width
  preffered-width ; preffered column width
  min-prefix-len ; min. prefix (spaces only)
  numeric ; y if column is numeric, n if is not, nil if don't know
)

(defun sqlplus-previous-line ()
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col t)))

(defun sqlplus-parse-output-table (interrupted)
  "Parse table and return list (COLUMN-INFOS ROWS MSG) where
COLUMN-INFOS is a col-desc structures list, ROWS is a table of
records (record is a list of strings).  Return nil if table is
not detected."
  (let ((region (sqlplus-find-output-table interrupted)))
    (when region
      (let ((begin (car region))
	    (end (cadr region))
	    (last-msg (caddr region))
	    (col-counter 0)
	    column-infos rows
	    (record-lines 1)
	    finish)
	(goto-char begin)
	;; we are at the first char of column name
        ;; move to the first char of '-----' column separator
	(beginning-of-line 2)
	(while (not finish)
	  (if (equal (char-after) ?-)
              ;; at the first column separator char
	      (let* ((beg (point))
		     (col-begin (current-column))
		     (col-max-width (skip-chars-forward "-"))
                     ;; after last column separator char
		     (ed (point))
		     (col-end (+ col-begin col-max-width))
		     (col-name (let* ((b (progn
					   (goto-char beg)
					   (sqlplus-previous-line)
					   (point)))
				      (e (+ b col-max-width)))
				 (skip-chars-forward " \t")
				 (setq b (point))
				 (goto-char (min (save-excursion (end-of-line) (point)) e))
				 (skip-chars-backward " \t")
				 (setq e (point))
				 (if (> e b)
				     (buffer-substring b e)
				   "")))
		     (col-preffered-width (length col-name)))
		;; (put-text-property 0 (length col-name) 'face '(bold) col-name)
		(push (make-col-desc :id col-counter :name col-name :start-pos col-begin
				     :end-pos col-end :max-width col-max-width :preffered-width col-preffered-width :min-prefix-len col-max-width)
		      column-infos)
		(incf col-counter)
		(goto-char ed)
		(if (equal (char-after) ?\n)
		    (progn
		      (beginning-of-line 3)
		      (incf record-lines))
		  (forward-char)))
	    (setq finish t)))
	(decf record-lines)
	(setq column-infos (nreverse column-infos))
	(forward-line -1)

        ;; at the first char of first data cell.
	;; table parsing...
	(while (< (point) end)
	  (let (record last-start-pos)
	    (dolist (column-info column-infos)
	      (let ((start-pos (col-desc-start-pos column-info))
		    (end-pos (col-desc-end-pos column-info))
		    len value b e l)
		(when (and last-start-pos
			   (<= start-pos last-start-pos))
		  (forward-line))
		(setq last-start-pos start-pos)
		(move-to-column start-pos)
		(setq b (point))
		(move-to-column end-pos)
		(setq e (point))
		(move-to-column start-pos)
		(setq l (skip-chars-forward " " e))
		(when (and (col-desc-min-prefix-len column-info)
			   (< l (- e b))
			   (< l (col-desc-min-prefix-len column-info)))
		  (setf (col-desc-min-prefix-len column-info)
			(if (looking-at "[0-9]") l nil)))
		(move-to-column end-pos)
		(skip-chars-backward " " b)
		(setq value (if (> (point) b) (buffer-substring b (point)) ""))
		(setq len (length value))
		(when (and sqlplus-select-result-max-col-width
			   (> len sqlplus-select-result-max-col-width))
		  (setq value (concat (substring value 0 sqlplus-select-result-max-col-width) "..."))
		  (setq len (length value)))
		(when (> len (col-desc-preffered-width column-info))
		  (setf (col-desc-preffered-width column-info) len))
                (when (and (< l (- e b))
                           (memq (col-desc-numeric column-info) '(nil y)))
                  (setf (col-desc-numeric column-info)
                        (if (string-match "\\` *[-+0-9Ee.,$]+\\'" value) 'y 'n)))
		(push value record)))
	    (forward-line)
	    (when (> record-lines 1)
	      (forward-line))
	    (setq last-start-pos nil)
	    (setq record (nreverse record))
	    (push record rows)))
	(setq rows (nreverse rows))
	(list column-infos rows last-msg)))))

(defun sqlplus-draw-table (lst &optional slips-count)
  "SLIPS-COUNT (nil means compute automatically)."
  ;; current buffer: SQL*Plus output buffer
  (if (>= (sqlplus-color-percentage (face-background 'default)) 50)
      (progn
        (set-face-attribute 'sqlplus-table-head-face nil
                            :background (sqlplus-shine-color (face-background 'default) -70) :foreground (face-background 'default))
        (set-face-attribute 'sqlplus-table-even-rows-face nil
                            :background (sqlplus-shine-color (face-background 'default) -20) :overline (face-background 'default))
        (set-face-attribute 'sqlplus-table-odd-rows-face nil
                            :background (sqlplus-shine-color (face-background 'default) -30) :overline (face-background 'default)))
    (set-face-attribute 'sqlplus-table-head-face nil
                        :background (sqlplus-shine-color (face-background 'default) +70) :foreground (face-background 'default))
    (set-face-attribute 'sqlplus-table-even-rows-face nil
                        :background (sqlplus-shine-color (face-background 'default) +20) :overline (face-background 'default))
    (set-face-attribute 'sqlplus-table-odd-rows-face nil
                        :background (sqlplus-shine-color (face-background 'default) +30) :overline (face-background 'default)))
  (let* ((column-infos (car lst))
         (rows (cadr lst))
         (slip-width 0)
         (table-header-height 1)
         (table-area-width (1- (let ((side-window (sqlplus-get-side-window))) (if side-window (window-width side-window) (frame-width)))))
         ;; may be nil, which means no limit
         (table-area-height (let ((side-window (sqlplus-get-side-window)))
                              (when side-window
                                (- (window-height side-window) 2 (if mode-line-format 1 0) (if header-line-format 1 0)))))
         (column-separator-width (if sqlplus-elegant-style 1.2 (max (length sqlplus-table-col-separator) (length sqlplus-table-col-head-separator))))
         rows-per-slip ;; data rows per slip
         (slip-separator-width (if sqlplus-elegant-style 1.5 sqlplus-slip-separator-width))
         (slip-separator (make-string (max 0 (if sqlplus-elegant-style 1 sqlplus-slip-separator-width)) ?\ ))
         (last-msg (caddr lst)))
    (when sqlplus-elegant-style
      (put-text-property 0 1 'display (cons 'space (list :width slip-separator-width)) slip-separator))
    (when (<= table-area-height table-header-height)
      (setq table-area-height nil))
    (when (and window-system sqlplus-elegant-style table-area-height (> table-area-height 3))
      ;; overline makes glyph higher...
      (setq table-area-height (- table-area-height (round (/ (* 20.0 (- table-area-height 3)) (face-attribute 'default :height))))))
    (when column-infos
      (goto-char (point-max))
      (beginning-of-line)
      ;; slip width (without separator between slips)
      (dolist (col-info column-infos)
        (when (col-desc-min-prefix-len col-info)
          (setf (col-desc-preffered-width col-info) (max (length (col-desc-name col-info))
                                                         (- (col-desc-preffered-width col-info) (col-desc-min-prefix-len col-info)))))
	(incf slip-width (+ (col-desc-preffered-width col-info) column-separator-width)))
      (when (> slip-width 0)
        (setq slip-width (+ (- slip-width column-separator-width) (if sqlplus-elegant-style 1.0 0))))
      ;; computing slip count if not known yet
      (unless slips-count
	(setq slips-count
	      (if table-area-height (min (ceiling (/ (float (length rows)) (max 1 (- table-area-height table-header-height 2))))
					 (max 1 (floor (/ (float table-area-width) (+ slip-width slip-separator-width)))))
		1)))
      (setq slips-count (max 1 (min slips-count (length rows)))) ; slip count <= data rows
      (setq rows-per-slip (ceiling (/ (float (length rows)) slips-count)))
      (when (> rows-per-slip 0)
        (setq slips-count (max 1 (min (ceiling (/ (float (length rows)) rows-per-slip)) slips-count))))

      (let ((table-begin-point (point)))
	(dotimes (slip-no slips-count)
	  (let ((row-no 0)
		(slip-begin-point (point))
		(rows-processed 0))
	    ;; column names
	    (dolist (col-info column-infos)
	      (let* ((col-name (col-desc-name col-info))
		     (spaces (max 0 (- (col-desc-preffered-width col-info) (length col-name))))
                     (last-col-p (>= (1+ (col-desc-id col-info)) (length column-infos)))
		     (val (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                  col-name
                                  (make-string spaces ?\ )
				  (if last-col-p "" (if sqlplus-elegant-style " " sqlplus-table-col-separator)))))
                (put-text-property 0 (if (or (not sqlplus-elegant-style) last-col-p) (length val) (1- (length val))) 
                                   'face 'sqlplus-table-head-face val)
                (when sqlplus-elegant-style
                  (put-text-property 0 1 'display '(space . (:width 0.5)) val)
                  (put-text-property (- (length val) (if last-col-p 1 2)) (- (length val) (if last-col-p 0 1)) 'display '(space . (:width 0.5)) val)
                  (unless last-col-p
                    (put-text-property (- (length val) 1) (length val) 'display '(space . (:width 0.2)) val)))
		(insert val)))
	    (insert slip-separator)
	    (insert "\n")
	    ;; data rows
	    (while (and (< rows-processed rows-per-slip)
			rows)
	      (let ((row (car rows)))
		(setq rows (cdr rows))
		(incf rows-processed)
		(let ((col-infos column-infos))
		  (dolist (value row)
		    (let* ((col-info (car col-infos))
			   (numeric-p (eq (col-desc-numeric col-info) 'y))
			   (min-prefix (col-desc-min-prefix-len col-info)))
		      (when (and min-prefix
				 value
				 (>= (length value) min-prefix))
			(setq value (substring value min-prefix)))
		      (let* ((spaces (max 0 (- (col-desc-preffered-width col-info) (length value))))
			     (val (if numeric-p
				      (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                              (make-string spaces ?\ )
                                              value
                                              (if (cdr col-infos) (if sqlplus-elegant-style " " sqlplus-table-col-separator) ""))
				    (format (if sqlplus-elegant-style " %s%s %s" "%s%s%s")
                                            value
                                            (make-string spaces ?\ ) 
                                            (if (cdr col-infos) (if sqlplus-elegant-style " " sqlplus-table-col-separator) "")))))
			(put-text-property 0 (if (and sqlplus-elegant-style (cdr col-infos)) (- (length val) 1) (length val))
                                           'face (if (evenp row-no)
                                                     'sqlplus-table-even-rows-face
                                                   'sqlplus-table-odd-rows-face) val)
                        (when sqlplus-elegant-style
                          (put-text-property 0 1 'display '(space . (:width 0.5)) val)
                          (put-text-property (- (length val) (if (cdr col-infos) 2 1))
                                             (- (length val) (if (cdr col-infos) 1 0))
                                             'display '(space . (:width 0.5)) val)
                          (when (cdr col-infos)
                            (put-text-property (- (length val) 1) (length val) 'display '(space . (:width 0.2)) val)))
			(setq col-infos (cdr col-infos))
			(insert val))))
		  (incf row-no)
		  (insert slip-separator)
		  (insert "\n"))))
	    (when (> slip-no 0)
	      (delete-backward-char 1)
	      (let ((slip-end-point (point)))
		(kill-rectangle slip-begin-point slip-end-point)
		(delete-region slip-begin-point (point-max))
		(goto-char table-begin-point)
		(end-of-line)
		(yank-rectangle)
		(goto-char (point-max))
		))))
	(goto-char (point-max))
	(when (and last-msg (> (length last-msg) 0))
          (unless sqlplus-elegant-style (insert "\n"))
          (let ((s (format "%s\n\n" (replace-regexp-in-string "\n+" " " last-msg))))
            (when sqlplus-elegant-style
              (put-text-property (- (length s) 2) (1- (length s)) 'display '(space . (:height 1.5)) s))
            (insert s)))))))

(defun sqlplus-send-user-string (str)
  (interactive (progn (sqlplus-check-connection)
                      (if sqlplus-connect-string
                          (list (read-string "Send to process: " nil 'sqlplus-user-string-history ""))
                        (error "Works only in SQL*Plus buffer"))))
  (let ((connect-string sqlplus-connect-string))
    (sqlplus-verify-buffer connect-string)
    (let* ((process (get-process (sqlplus-get-process-name connect-string)))
           (output-buffer-name (sqlplus-get-output-buffer-name connect-string)))
      (sqlplus-echo-in-buffer output-buffer-name (concat str "\n"))
      (send-string process (concat str "\n")))))

(defun sqlplus-set-font-lock-data (regexp-map)
  (setq sqlplus-font-lock-regexps regexp-map)
  (when font-lock-mode
    (font-lock-mode 1)))

(defun sqlplus-my-handler (connect-string table-data)
  (let ((column-infos (car table-data))
        (rows (cadr table-data))
        (msg (caddr table-data))
        alist)
    (clrhash sqlplus-font-lock-regexps)
    (dolist (row rows)
      (let* ((object-name (car row))
             (object-type (intern (downcase (cadr row))))
             (regexp-list (cdr (assq object-type alist))))
        (if regexp-list
            (setcdr regexp-list (cons object-name (cdr regexp-list)))
          (setq regexp-list (list object-name))
          (setq alist (cons (cons object-type regexp-list) alist)))))
    (dolist (lst sqlplus-syntax-faces)
      (let* ((object-type (car lst))
             (regexp-list (append (caddr lst) (cdr (assq object-type alist)))))
        (when regexp-list
          (puthash object-type (concat "\\b" (regexp-opt regexp-list t) "\\b") sqlplus-font-lock-regexps))))
    (let ((map sqlplus-font-lock-regexps))
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (when (and (memq major-mode '(sqlplus-mode plsql-mode))
                           (equal sqlplus-connect-string connect-string))
                  (sqlplus-set-font-lock-data map))))
            (buffer-list)))))

(defun sqlplus-get-source-function (connect-string context string last-chunk)
  (let ((source-buffer (sqlplus-get-context-value context :source-buffer))
        (source-type (sqlplus-get-context-value context :source-type))
        (source-name (sqlplus-get-context-value context :source-name)))
    (with-current-buffer source-buffer
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (save-excursion
              (goto-char (point-max))
              (insert string)
              (when last-chunk
                (goto-char (point-min))
                (if (re-search-forward (regexp-quote sqlplus-end-of-source-sentinel) nil t)
                    (progn
                      (replace-match "")
                      (when (<= (buffer-size) 1)
                        (save-buffer)
                        (kill-buffer (current-buffer))
                        (setq last-chunk nil)
                        (message "There is no such source code.")))
                  (setq last-chunk nil)))
              (when last-chunk
                ;; (rename-buffer (substring (buffer-name) 1) t)
                (goto-char (point-max))
                (forward-comment (- (buffer-size)))
                (when (equal source-type "TABLE")
                  (goto-char (point-min))
                  (insert (format "table %s\n(\n" source-name))
                  (goto-char (point-max))
                  (delete-region (re-search-backward "," nil t) (point-max))
                  (insert "\n);"))
                (insert "\n/\n")
                (unless (member source-type '("SEQUENCE" "TABLE" "SYNONYM" "INDEX"))
                  (insert "show err\n"))
                (goto-char (point-min))
                (insert "create " (if (member source-type '("INDEX" "SEQUENCE" "TABLE")) "" "or replace "))
                (save-buffer)))
            (when last-chunk
              (save-selected-window
                (when (equal (selected-window) (sqlplus-get-side-window))
                  (select-window (sqlplus-get-workbench-window)))
                (switch-to-buffer source-buffer)))))
      (when (eq (current-buffer) source-buffer)
        (setq buffer-read-only sqlplus-source-buffer-readonly-by-default-flag)))))

(defun sqlplus-get-source (connect-string name type &optional schema-name)
  "Fetch source for database object NAME in current or specified SCHEMA-NAME, and show the source in new buffer.
Possible TYPE values are in 'sqlplus-object-types'."
  (interactive (let* ((thing (thing-at-point 'symbol))
                      (obj-raw-name (read-string (concat "Object name" (if thing (concat " [default " thing "]") "") ": ")
                                                 nil
                                                 'sqlplus-get-source-history (when thing thing)))
                      (completion-ignore-case t)
                      (type (completing-read "Object type: " (mapcar (lambda (type) (cons type nil)) sqlplus-object-types) nil t)))
                 (string-match "^\\(\\([^.]+\\)[.]\\)?\\(.*\\)$" obj-raw-name)
                 (list sqlplus-connect-string (match-string 3 obj-raw-name) type (match-string 2 obj-raw-name))))
  (setq type (upcase type))
  (let* ((sql
          (cond ((equal type "SEQUENCE")
                 (format (concat "select 'sequence %s' || sequence_name || "
                                 "decode( increment_by, 1, '', ' increment by ' || increment_by ) || "
                                 "case when increment_by > 0 and max_value >= (1.0000E+27)-1 or increment_by < 0 and max_value = -1 then '' "
                                 "else decode( max_value, null, ' nomaxvalue', ' maxvalue ' || max_value) end || "
                                 "case when increment_by > 0 and min_value = 1 or increment_by < 0 and min_value <= (-1.0000E+26)+1 then '' "
                                 "else decode( min_value, null, ' nominvalue', ' minvalue ' || min_value) end || "
                                 "decode( cycle_flag, 'Y', ' cycle', '' ) || "
                                 "decode( cache_size, 20, '', 0, ' nocache', ' cache ' || cache_size ) || "
                                 "decode( order_flag, 'Y', ' order', '' ) "
                                 "from %s where sequence_name = '%s'%s;")
                         (if schema-name (concat (upcase schema-name) ".") "")
                         (if schema-name "all_sequences" "user_sequences")
                         (upcase name)
                         (if schema-name (format " and sequence_owner = '%s'" (upcase schema-name)) "")))
                ((equal type "TABLE")
                 (format (concat "select '  ' || column_name || ' ' || data_type || "
                                 "decode( data_type,"
                                 " 'VARCHAR2', '(' || to_char( data_length, 'fm9999' ) || ')',"
                                 " 'NUMBER', decode( data_precision,"
                                 "             null, '',"
                                 "             '(' || to_char( data_precision, 'fm9999' ) || decode( data_scale,"
                                 "                                                      null, '',"
                                 "                                                      0, '',"
                                 "                                                      ',' || data_scale ) || ')' ),"
                                 " '') || "
                                 "decode( nullable, 'Y', ' not null', '') || ','"
                                 "from all_tab_columns "
                                 "where owner = %s and table_name = '%s' "
                                 "order by column_id;")
                         (if schema-name (concat "'" (upcase schema-name) "'") "user")
                         (upcase name)))
                ((equal type "SYNONYM")
                 (format (concat "select "
                                 "decode( owner, 'PUBLIC', 'public ', '' ) || 'synonym ' || "
                                 "decode( owner, 'PUBLIC', '', user, '', owner || '.' ) || synonym_name || ' for ' || "
                                 "decode( table_owner, user, '', table_owner || '.' ) || table_name || "
                                 "decode( db_link, null, '', '@' || db_link ) "
                                 "from all_synonyms where (owner = 'PUBLIC' or owner = %s) and synonym_name = '%s';")
                         (if schema-name (concat "'" (upcase schema-name) "'") "user")
                         (upcase name)))
                ((equal type "VIEW")
                 (if schema-name (format "select 'view %s.' || view_name || ' as ', text from all_views where owner = '%s' and view_name = '%s';"
                                         (upcase schema-name) (upcase schema-name) (upcase name))
                   (format "select 'view ' || view_name || ' as ', text from user_views where view_name = '%s';" (upcase name))))
                (t
                 (if schema-name (format "select text from all_source where owner = '%s' and name = '%s' and type = '%s' order by line;"
                                         (upcase schema-name) (upcase name) (upcase type))
                   (format "select text from user_source where name = '%s' and type = '%s' order by line;"
                           (upcase name) (upcase type))))))
         (prolog-commands (list "set echo off"
                                "set newpage 0"
                                "set space 0"
                                "set pagesize 0"
                                "set feedback off"
                                "set long 4000"
                                "set longchunksize 400"
                                "set wrap on"
                                "set heading off"
                                "set trimspool on"
                                "set linesize 400"
                                "set timing off"))
         (extension (if (equal (downcase type) "package") "pks" "sql"))
         (source-buffer-name (concat (upcase name) "." extension))
         (source-buffer (generate-new-buffer source-buffer-name))
         (context-options (list (cons :dont-parse-result 'dont-parse)
                                (cons :source-buffer source-buffer)
                                (cons :source-type type)
                                (cons :source-name name)
                                (cons :result-function 'sqlplus-get-source-function))))
    (setq source-buffer-name (buffer-name source-buffer))
    (with-current-buffer source-buffer
      (funcall (symbol-function 'plsql-mode))
      (setq sqlplus-connect-string connect-string)
      (erase-buffer)
      (set-visited-file-name (concat (file-name-as-directory temporary-file-directory)
                                     (concat (make-temp-name (sqlplus-canonize-file-name (concat (upcase name) "_") "[$]")) "." extension)))
      (rename-buffer source-buffer-name))
    (sqlplus-execute connect-string sql context-options prolog-commands t t)
    (sqlplus-execute connect-string (format "select '%s' from dual;" sqlplus-end-of-source-sentinel) context-options prolog-commands t t)
    ))

(defun sqlplus-canonize-file-name (file-name regexp)
  (while (string-match regexp file-name)
    (setq file-name (replace-match "!" nil t file-name)))
  file-name)

(defun sqlplus-define-user-variables (string)
  (when string
    (let (variables-list
          define-commands
          (index 0))
      (while (setq index (string-match "&+\\(\\(\\sw\\|\\s_\\)+\\)" string index))
        (let ((var-name (match-string 1 string)))
          (setq index (+ 2 index))
          (unless (member var-name variables-list)
            (push var-name variables-list))))
      (dolist (var-name (reverse variables-list))
        (let* ((default-value (gethash var-name sqlplus-user-variables nil))
               (value (read-string (format (concat "Variable value for %s" (if default-value (format " [default: %s]" default-value) "") ": ") var-name)
                                   nil 'sqlplus-user-variables-history default-value)))
          (unless value
            (error "There is no value for %s defined" var-name))
          (setq define-commands (cons (format "define %s=%s" var-name value) define-commands))
          (puthash var-name value sqlplus-user-variables)))
      define-commands)))
    
(defun sqlplus-parse-region (start end)
  (let ((sql (buffer-substring start end)))
    (save-excursion
      ;; Strip whitespace from beginning and end, just to be neat.
      (if (string-match "\\`[ \t\n]+" sql)
          (setq sql (substring sql (match-end 0))))
      (if (string-match "[ \t\n]+\\'" sql)
          (setq sql (substring sql 0 (match-beginning 0))))
      ;; Now the string should end with an sqlplus-terminator.
      (if (not (string-match "\\(;\\|/\\|[.]\\)\\'" sql))
          (setq sql (concat sql ";"))))
    sql))

(defun sqlplus-show-html-fun (context connect-string begin end interrupted)
  (let ((output-file (expand-file-name (substitute-in-file-name sqlplus-html-output-file-name)))
        (sql (sqlplus-get-context-value context :htmlized-html-command))
        (html (buffer-substring begin end))
        (header-html (eval sqlplus-html-output-header)))
    (let ((case-fold-search t))
      (while (and (string-match "\\`[ \t\n]*\\(<br>\\|<p>\\)?" html) (match-string 0 html) (> (length (match-string 0 html)) 0))
        (setq html (replace-match "" nil t html)))
      (when (> (length html) 0)
        (sqlplus-execute connect-string "" nil '("set markup html off") 'no-echo 'dont-show-output-buffer)
        (find-file output-file)
        (erase-buffer)
        (insert (concat "<html>\n"
                        "<head>\n"
                        "  <meta http-equiv=\"content-type\" content=\"text/html; charset=" sqlplus-html-output-encoding "\">\n"
                        (sqlplus-get-context-value context :head) "\n"
                        "</head>\n"
                        "<body " (sqlplus-get-context-value context :body) ">\n"
                        (if header-html header-html "")
                        (if sqlplus-html-output-sql sql "")
                        "<p>"
                        html "\n"
                        "</body>\n"
                        "</html>"))
        (goto-char (point-min))
        (save-buffer)))))

(defun sqlplus-refine-html (html remove-entities)
  (string-match "\\`\"?\\(\\(.\\|\n\\)*?\\)\"?[ \t\n]*\\'" html)
  (setq html (match-string 1 html))
  (if remove-entities
      (progn
        (while (string-match "&quot;" html) (setq html (replace-match "\"" nil t html)))
        (while (string-match "&lt;" html) (setq html (replace-match "<" nil t html)))
        (while (string-match "&gt;" html) (setq html (replace-match ">" nil t html)))
        (while (string-match "&amp;" html) (setq html (replace-match "&" nil t html))))
    (while (string-match "&" html) (setq html (replace-match "&amp;" nil t html)))
    (while (string-match ">" html) (setq html (replace-match "&gt;" nil t html)))
    (while (string-match "<" html) (setq html (replace-match "&lt;" nil t html)))
    (while (string-match "\"" html) (setq html (replace-match "&quot;" nil t html))))
  (string-match "\\`\"?\\(\\(.\\|\n\\)*?\\)\"?[ \t\n]*\\'" html)
  (setq html (match-string 1 html))
  html)

(defun sqlplus-show-markup-fun (context connect-string begin end interrupted)
  (goto-char begin)
  (let ((head "")
        (body "")
        preformat)
    (when (re-search-forward (concat "\\bHEAD\\b[ \t\n]*\\(\\(.\\|\n\\)*\\)[ \t\n]*"
                                     "\\bBODY\\b[ \t\n]*\\(\\(.\\|\n\\)*\\)[ \t\n]*"
                                     "\\bTABLE\\b\\(.\\|\n\\)*PREFORMAT[ \t\n]+\\(ON\\|OFF\\)\\b") nil t)
      (setq head (match-string 1)
            body (match-string 3)
            preformat (string= (downcase (match-string 6)) "on"))
      (setq head (sqlplus-refine-html head t)
            body (sqlplus-refine-html body t))
      (let ((context-options (list (cons :result-function 'sqlplus-show-html-fun)
                                   (cons :current-command-input-buffer-name (sqlplus-get-context-value context :current-command-input-buffer-name))
                                   (cons :html-command (sqlplus-get-context-value context :html-command))
                                   (cons :htmlized-html-command (sqlplus-get-context-value context :htmlized-html-command))
                                   (cons :head head)
                                   (cons :body body)))
            (prolog-commands (list "set wrap on"
                                   (format "set linesize %S" (if preformat (1- (frame-width)) 4000))
                                   "set pagesize 50000"
                                   "btitle off"
                                   "repfooter off"
                                   "set markup html on")))
        (sqlplus-execute connect-string (sqlplus-get-context-value context :html-command) context-options prolog-commands 'no-echo 'dont-show-output-buffer)))))

(defun sqlplus-htmlize (begin end)
  (let (result)
    (when (featurep 'htmlize)
      (let* ((htmlize-output-type 'font)
             (buffer (funcall (symbol-function 'htmlize-region) begin end)))
        (with-current-buffer buffer
          (goto-char 1)
          (re-search-forward "<pre>[ \t\n]*\\(\\(.\\|\n\\)*?\\)[ \t\n]*</pre>" nil t)
          (setq result (concat "<pre>" (match-string 1) "</pre>")))
        (kill-buffer buffer)))
    (unless result
      (setq result (sqlplus-refine-html (buffer-substring begin end) nil)))
    result))

(defun sqlplus-send-region (arg start end &optional no-echo html)
  "Send a region to the SQL*Plus process."
  (interactive "Pr")
  (sqlplus-check-connection)
  (if html
      (let* ((sql (sqlplus-parse-region start end))
             (context-options (list (cons :result-function 'sqlplus-show-markup-fun)
                                    (cons :current-command-input-buffer-name (buffer-name))
                                    (cons :html-command sql)
                                    (cons :htmlized-html-command (if (and (eq sqlplus-html-output-sql 'elegant) (featurep 'htmlize))
                                                                     (sqlplus-htmlize start end)
                                                                   (sqlplus-refine-html sql nil))))))
        (sqlplus-execute sqlplus-connect-string "show markup\n" context-options nil 'no-echo 'dont-show-output-buffer))
    (let* ((no-parse (consp arg))
           (context-options (list (cons :dont-parse-result (consp arg))
                                  (cons :columns-count (if (integerp arg)
                                                           (if (zerop arg) nil arg)
                                                         (if sqlplus-multi-output-tables-default-flag nil 1)))
                                  (cons :current-command-input-buffer-name (buffer-name))))
           (prolog-commands (list (format "set wrap %s" (if no-parse "on" sqlplus-default-wrap))
                                  (format "set linesize %s" (if (consp arg) (1- (frame-width)) 4000))
                                  (format "set pagesize %S" (if no-parse 50000 sqlplus-pagesize))
                                  (format "btitle %s"
                                          (if no-parse "off" (concat "left '" sqlplus-page-separator "'")))
                                  (format "repfooter %s"
                                          (if no-parse "off" (concat "left '" sqlplus-repfooter "'")))))
           (sql (sqlplus-parse-region start end)))
      (sqlplus-execute sqlplus-connect-string sql context-options prolog-commands no-echo))))

(defun sqlplus-hidden-select (connect-string sql result-proc)
  (let* ((context-options (list (cons :result-table-function result-proc)
                                (cons :columns-count 1)))
        (prolog-commands (list (format "set wrap %s" sqlplus-default-wrap)
                               "set linesize 4000"
                               "set pagesize 50000"
                               "btitle off"
                               (format "repfooter %s" (concat "left '" sqlplus-repfooter "'")))))
    (sqlplus-execute connect-string sql context-options prolog-commands 'no-echo 'dont-show-output-buffer)))

;; "appi[nfo]" -> '("appinfo" "appi")
(defun sqlplus-full-forms (name)
  (if (string-match "\\`\\([^[]*\\)?\\[\\([^]]+\\)\\]\\([^]]*\\)?\\'" name)
      (list (replace-match "\\1\\2\\3" t nil name)
            (replace-match "\\1\\3" t nil name))
    (list name)))

(defun sqlplus-get-canonical-command-name (name)
  (let ((association (assoc (downcase name) sqlplus-system-variables)))
    (if association (cdr association) name)))
    

(defun sqlplus-execute (connect-string sql context-options prolog-commands &optional no-echo dont-show-output-buffer)
  (sqlplus-verify-buffer connect-string)

  (let* ((process-buffer-name (sqlplus-get-process-buffer-name connect-string))
         (process-buffer (get-buffer process-buffer-name))
         (output-buffer-name (sqlplus-get-output-buffer-name connect-string))
         (echo-prolog (concat "\n" sqlplus-output-separator " " (current-time-string) "\n\n"))
         (process  (get-buffer-process process-buffer-name))
         set-prolog-commands commands command-no
         (defines (sqlplus-define-user-variables sql)))
    (setq prolog-commands (append (sqlplus-initial-strings) prolog-commands))
    (when process-buffer
      (with-current-buffer process-buffer
        (let* (set-prolog-commands commands)
          (setq command-no sqlplus-command-seq)
          (setq sqlplus-command-seq (1+ sqlplus-command-seq))
          (setq context-options (cons (cons :id command-no) (copy-list context-options)))
          (setq sqlplus-command-contexts (reverse (cons context-options (reverse sqlplus-command-contexts)))))))
    (setq prolog-commands (delq nil (mapcar (lambda (command) (if (string-match "^\\s-*[sS][eE][tT]\\s-+" command)
                                                                  (progn
                                                                    (setq set-prolog-commands
                                                                          (append set-prolog-commands
                                                                                  (list (substring command (length (match-string 0 command))))))
                                                                    nil)
                                                                command))
                                            prolog-commands)))
    (let (spc-alist)
      (dolist (command prolog-commands)
        (let* ((name (progn (string-match "^\\S-+" command) (downcase (match-string 0 command))))
               (association (assoc name spc-alist)))
          (if association
              (setcdr association command)
            (setq spc-alist (cons (cons name command) spc-alist)))))
      (setq prolog-commands (mapcar (lambda (pair) (cdr pair)) (reverse spc-alist))))

    (setq prolog-commands (append prolog-commands defines))
    (setq set-prolog-commands (append (list (format "sqlprompt '%s%S%s'" sqlplus-prompt-prefix command-no sqlplus-prompt-suffix)) set-prolog-commands))

    ;; usuwamy duplikaty z set-prolog-commands (ostatnie wpisy wygrywaja)
    (let (spc-alist)
      (dolist (set-command set-prolog-commands)
        (let* ((name (progn (string-match "^\\S-+" set-command) (downcase (sqlplus-get-canonical-command-name (match-string 0 set-command)))))
               (association (assoc name spc-alist)))
          (if association
              (setcdr association set-command)
            (setq spc-alist (cons (cons name set-command) spc-alist)))))
      (setq set-prolog-commands (mapcar (lambda (pair) (cdr pair)) (reverse spc-alist))))
          
    (setq commands (concat (mapconcat 'identity (append (list (concat "set " (mapconcat 'identity set-prolog-commands " "))) prolog-commands (list sql)) "\n")
                           "\n"))
    (with-current-buffer (sqlplus-get-history-buffer connect-string)
      (goto-char (point-max))
      (insert echo-prolog)
      (insert (concat commands "\n")))
    (unless no-echo
      (sqlplus-echo-in-buffer output-buffer-name echo-prolog)
      (let ((old-suppress-show-output-buffer sqlplus-suppress-show-output-buffer))
        (unwind-protect
            (progn
              (setq sqlplus-suppress-show-output-buffer dont-show-output-buffer)
              (sqlplus-buffer-bottom connect-string)
              (sqlplus-buffer-mark-current connect-string))
          (setq sqlplus-suppress-show-output-buffer old-suppress-show-output-buffer)))
      (sqlplus-echo-in-buffer output-buffer-name (concat sql "\n\n") nil t)
      (unless dont-show-output-buffer
        (sqlplus-buffer-redisplay-current connect-string)))
    (send-string process commands)))

(unless plsql-mode-map
  (setq plsql-mode-map (copy-keymap sql-mode-map))
  (define-key plsql-mode-map "\C-c\C-g" 'plsql-begin)
  (define-key plsql-mode-map "\C-c\C-q" 'plsql-loop)
  (define-key plsql-mode-map "\C-c\C-z" 'plsql-if)
  (define-key plsql-mode-map "\C-c\C-c" 'plsql-compile))

(defvar plsql-continue-anyway nil)
(make-variable-buffer-local 'plsql-continue-anyway)

(defun plsql-compile (&optional arg)
  "Save buffer and send its content to SQL*Plus.
You must enter connect-string if buffer is disconnected; with
argument you can change connect-string even for connected
buffer."
  (interactive "P")
  (let (aborted
        exists-show-error-command
        (case-fold-search t))
    (save-window-excursion
      (save-excursion
        ;; ask for "/" and "show err" if absent
        (let ((old-point (point))
              show-err-needed
              exists-run-command best-point finish)
          (goto-char (point-min))
          (setq show-err-needed (let ((case-fold-search t))
                                  (re-search-forward "create\\([ \t\n]+or[ \t\n]+replace\\)?[ \t\n]+\\(package\\|procedure\\|function\\|trigger\\|view\\|type\\)" nil t)))
          (goto-char (point-max))
          (forward-comment (- (buffer-size)))
          (re-search-backward "^\\s-*show\\s-+err" nil t)
          (forward-comment (- (buffer-size)))
          (condition-case nil (forward-char) (error nil))
          (setq best-point (point))
          (goto-char (point-min))
          (setq exists-run-command (re-search-forward "^\\s-*/[^*]" nil t))
          (goto-char (point-min))
          (setq exists-show-error-command (or (not show-err-needed) (re-search-forward "^\\s-*show\\s-+err" nil t)))
          (while (and (not plsql-continue-anyway) (or (not exists-run-command) (not exists-show-error-command)) (not finish))
            (goto-char best-point)
            (let ((c (read-char 
                      (format "Cannot find %s.  (I)nsert it at point, (A)bort, (C)ontinue anyway"
                              (concat (unless exists-run-command "\"/\"")
                                      (unless (or exists-run-command exists-show-error-command) " and ")
                                      (unless exists-show-error-command "\"show err\""))))))
              (cond ((memq c '(?i ?I))
                     (unless exists-run-command (insert "/\n"))
                     (unless exists-show-error-command (insert "show err\n"))
                     (setq finish t))
                    ((memq c '(?a ?A))
                     (setq aborted t
                           finish t))
                    ((memq c '(?c ?C))
                     (setq plsql-continue-anyway t)
                     (setq finish t))))))))
    (unless aborted
      (save-buffer)
      (let* ((buffer (current-buffer))
             (input-buffer-name (buffer-name))
             (file-path (sqlplus-file-truename (buffer-file-name)))
             (compilation-buffer (get-buffer sqlplus-plsql-compilation-results-buffer-name))
             (context-options (list (cons :last-compiled-file-path file-path)
                                    (cons :current-command-input-buffer-name input-buffer-name)
                                    (cons :compilation-expected exists-show-error-command)))
             (prolog-commands (list (format "set wrap %s" sqlplus-default-wrap)
                                    "set linesize 4000"
                                    (format "set pagesize %S" sqlplus-pagesize)
                                    (format "btitle %s" (concat "left '" sqlplus-page-separator "'"))
                                    (format "repfooter %s" (concat "left '" sqlplus-repfooter "'")))))
        (when (or (not sqlplus-connect-string)
                  arg)
          (setq sqlplus-connect-string (car (sqlplus-read-connect-string nil (caar (sqlplus-divide-connect-strings))))))
        (sqlplus sqlplus-connect-string nil plsql-auto-parse-errors-flag)
        (set-buffer buffer)
        (force-mode-line-update)
        (let ((output-buffer (get-buffer (sqlplus-get-output-buffer-name sqlplus-connect-string)))
              regexp-map)
          (when output-buffer
            (with-current-buffer output-buffer
              (setq regexp-map sqlplus-font-lock-regexps)))
          (sqlplus-set-font-lock-data regexp-map))
        (when compilation-buffer
          (with-current-buffer compilation-buffer
            (setq buffer-read-only nil)
            (erase-buffer)
            (setq buffer-read-only t)))
        (setq prolog-commands (append prolog-commands (sqlplus-define-user-variables (buffer-string))))
        (sqlplus-execute sqlplus-connect-string (concat "@" file-path) context-options prolog-commands nil exists-show-error-command)))))

(defun plsql-parse-errors (last-compiled-file-path)
  (let ((file-name (file-name-nondirectory last-compiled-file-path))
        error-list)
    (put-text-property 0 (length file-name) 'face 'font-lock-warning-face file-name)
    (save-excursion 
      (when (re-search-forward "^LINE/COL\\>" nil t)
        (beginning-of-line 3)
        (while (re-search-forward "^\\([0-9]+\\)/\\([0-9]+\\)\\s-*\\(\\(.\\|\n\\)*?\\)[\r\t ]*\n\\([\r\t ]*\\(\n\\|\\'\\)\\|[0-9]+\\)" nil t)
          (let ((line-no (match-string 1))
                (column-no (match-string 2))
                (errmsg (match-string 3))
                label)
            (goto-char (match-beginning 5))
            (while (string-match "\\s-\\s-+" errmsg)
              (setq errmsg (replace-match " " nil t errmsg)))
            (put-text-property 0 (length line-no) 'face 'font-lock-variable-name-face line-no)
            (put-text-property 0 (length column-no) 'face 'font-lock-variable-name-face column-no)
            (setq label (concat file-name ":" line-no ":" column-no ": " errmsg))
            (put-text-property 0 (length label) 'mouse-face 'highlight label)
            (push label error-list)))))
    (save-excursion
      (while (re-search-forward "\\s-\\([0-9]+\\):\n\\(ORA-[0-9]+[^\n]*\\)\n" nil t)
        (let ((line-no (match-string 1))
              (errmsg (match-string 2))
              label)
          (put-text-property 0 (length line-no) 'face 'font-lock-variable-name-face line-no)
          (setq label (concat file-name ":" line-no ": " errmsg))
          (put-text-property 0 (length label) 'mouse-face 'highlight label)
          (push label error-list))))
    (save-excursion
      (while (re-search-forward "\\(\\(SP2\\|CPY\\)-[0-9]+:[^\n]*\\)\n" nil t)
        (let ((errmsg (match-string 1))
              label)
          (setq label (concat file-name ":" errmsg))
          (put-text-property 0 (length label) 'mouse-face 'highlight label)
          (push label error-list))))
    error-list))

(defun plsql-display-errors (dir error-list)
  (let ((buffer (get-buffer-create sqlplus-plsql-compilation-results-buffer-name)))
    (save-selected-window
      (save-excursion
        (set-buffer buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq default-directory dir)
        (insert (format "cd %s\n" default-directory))
        (insert (format "Compilation results\n"))
        (compilation-minor-mode 1)
        (dolist (msg (reverse error-list))
          (insert msg)
          (insert "\n"))
        (insert (format "\n(%s errors)\n" (length error-list)))
        (setq buffer-read-only t)
        (when (and error-list (fboundp 'compile-reinitialize-errors) (funcall (symbol-function 'compile-reinitialize-errors) t)))
        (switch-to-buffer-other-window buffer)
        (goto-line 1)
        (goto-line 3)))))


(defun sqlplus-file-truename (file-name)
  (if file-name
      (file-truename file-name)
    file-name))

(defun sqlplus--hidden-buffer-name-p (buffer-name)
  (equal (elt buffer-name 0) 32))

(defun sqlplus-get-workbench-window ()
  "Return upper left window"
  (if (fboundp 'ide-get-workbench-window)
      (funcall (symbol-function 'ide-get-workbench-window))
    (let (best-window)
      (dolist (win (copy-list (window-list nil 1)))
	(when (not (sqlplus--hidden-buffer-name-p (buffer-name (window-buffer win))))
	  (if (null best-window)
	      (setq best-window win)
	    (let* ((best-window-coords (window-edges best-window))
		   (win-coords (window-edges win)))
	      (when (or (< (cadr win-coords) (cadr best-window-coords))
			(and (= (cadr win-coords) (cadr best-window-coords))
			     (< (car win-coords) (car best-window-coords))))
		(setq best-window win))))))
      best-window)))

(defun sqlplus-get-side-window ()
  "Return bottom helper window, or nil if not found"
  (if (fboundp 'ide-get-side-window)
      (funcall (symbol-function 'ide-get-side-window))
    (let* ((workbench-window (sqlplus-get-workbench-window))
	   best-window)
      (dolist (win (copy-list (window-list nil 1)))
	(when (and (not (sqlplus--hidden-buffer-name-p (buffer-name (window-buffer win))))
		   (not (eq win workbench-window)))
	  (if (null best-window)
	      (setq best-window win)
	    (when (> (cadr (window-edges win)) (cadr (window-edges best-window)))
	      (setq best-window win)))))
      best-window)))

(defvar sqlplus--idle-tasks nil)

(defun sqlplus--enqueue-task (fun &rest params)
  (setq sqlplus--idle-tasks (reverse (cons (cons fun params) (reverse sqlplus--idle-tasks)))))

(defun sqlplus--execute-tasks ()
  (dolist (task sqlplus--idle-tasks)
    (let ((fun (car task))
          (params (cdr task)))
      (condition-case var
          (apply fun params)
        (error (message (error-message-string var))))))
  (setq sqlplus--idle-tasks nil))

(add-hook 'post-command-hook 'sqlplus--execute-tasks)


(add-hook 'plsql-mode-hook
          (lambda ()
            (modify-syntax-entry ?. "." sql-mode-syntax-table)
            (setq sqlplus-font-lock-keywords-1 (sqlplus-set-font-lock-emacs-structures-for-level 1 major-mode))
            (setq sqlplus-font-lock-keywords-2 (sqlplus-set-font-lock-emacs-structures-for-level 2 major-mode))
            (setq sqlplus-font-lock-keywords-3 (sqlplus-set-font-lock-emacs-structures-for-level 3 major-mode))
            (setq font-lock-defaults '((sqlplus-font-lock-keywords-1 sqlplus-font-lock-keywords-2 sqlplus-font-lock-keywords-3)
                                       nil t ((?_ . "w") (?$ . "w") (?# . "w") (?& . "w"))))
            (use-local-map plsql-mode-map) ; std
            (orcl-mode 1)))

(setq recentf-exclude (cons (concat "^" (regexp-quote (file-name-as-directory temporary-file-directory)))
			    (when (boundp 'recentf-exclude)
			      recentf-exclude)))

(when (fboundp 'ide-register-persistent-var)
  (funcall (symbol-function 'ide-register-persistent-var) 'sqlplus-connect-strings-alist
			       ;; save proc
			       (lambda (alist)
				 (mapcar (lambda (pair)
					   (if sqlplus-save-passwords
					       pair
					     (cons (car pair) nil)))
					 alist))
			       ;; load proc
			       (lambda (alist)
				 (setq sqlplus-connect-string-history (mapcar (lambda (pair) (car pair)) alist))
				 alist)))

(provide 'sqlplus)

;;; sqlplus.el ends here
