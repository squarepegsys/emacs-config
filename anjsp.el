;;; anjsp.el -- Ando jsp-mode for emacs.
;;<plaintext>

(defconst anjsp-revision-number "1.1.3" "ando jsp revision number")

;; Author: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Maintainer: Toshikazu Ando <ando@park.ruru.ne.jp>
;; Version: $Lastupdate: Sat Sep 08 13:58:49 2001 $ on inspire.
;; Created: 2001 Mar 1.
;; URL : http://park.ruru.ne.jp/ando/work/who/anjsp/
;; Keywords: jsp php yahtml HTML cc-mode epo JDE Ando

;;; Code:

;; Abstract.
;;
;; 1. Put into your ~/.emacs.
;;
;;for JSP...
;; (autoload 'anjsp-mode "anjsp" "ando jsp mode" t)
;; (setq auto-mode-alist
;;      (cons (cons "\\.jsp$" 'anjsp-mode) auto-mode-alist))
;;
;;for PHP...
;; (autoload 'anphp-mode "anjsp" "ando php mode" t)
;; (setq auto-mode-alist
;;      (cons (cons "\\.psp$" 'anphp-mode) auto-mode-alist))
;;
;; 2. Add certain path name where you put files of yancc to your
;;    load-path.  If you want to put them in ~/lisp
;;
;; (setq load-path
;;      (cons (expand-file-name "~/lisp") load-path))
;;
;;
;; Useing.
;;   Lets hit [TAB] key. Over?
;;

;; ヤってること
;;
;;     ようするに [tab] キーを打ったときにその部分が java か html かを
;;   調査して、メジャーモードを切り替えるとゆー、非常―に、めっちゃめっ
;;   ちゃ強引なことを行っております。
;;     色の切り替えは [tab] 後、C-l とかでお願いします。下手にいじくると
;;   めためた重いものができそうです。
;;     起動する基底メジャーモードを変えたい場合は anjsp-base-*-mode
;;   をsetq で変更してください。
;;
;------------------------------------------------------------------------
(require 'cc-mode)

;;; emacs 19 系でエラーが出たときは下記を消してください。
;;; そして anjsp の設定を最後に置いてください。
(require 'yahtml    nil t)
(require 'epojava   nil t)
(require 'jde       nil t)
(require 'perl-mode nil t)
;---------------------------------------------------------------------

;; default base mode
(defvar anjsp-base-java-mode 'epojava-mode "*java base mode")
(defvar anjsp-base-php-mode  'perl-mode    "*php base mode")
(defvar anjsp-base-html-mode 'yahtml-mode  "*html base mode")

;---------------------------------------------------------------------
(defvar anjsp-mode-map nil "epocc keymap")
(defconst anjsp-java-mode "java-mode"    "jsp-java identifer")
(defconst anjsp-php-mode  "php-mode"     "jsp-php  identifer")
(defconst anjsp-html-mode "yahtml-mode"  "jsp-html identifer")
(defvar anjsp-base-mode anjsp-html-mode  "now base mode")
(defvar anjsp-key ?% "now target key")

;---------------------------------------------------------------------

(defun anjsp-mode ()
  "ando jsp mode"
  (interactive)
  (anjsp-set-base-major-mode (anjsp-check ?%) ?%)
  (run-hooks 'anjsp-mode-hook))

(defun anphp-mode ()
  "ando php mode"
  (interactive)
  (anjsp-set-base-major-mode (anjsp-check ??) ??)
  (run-hooks 'anjsp-mode-hook))

(defun anjsp-tab-key ()
  "invoke tab key"
  (interactive)
  (let ((nowMode (anjsp-check anjsp-key)))
    (if (eq nowMode anjsp-base-mode)
	nil ; no change
      (anjsp-set-base-major-mode nowMode anjsp-key)))
  (if (and indent-line-function (fboundp indent-line-function))
    (funcall indent-line-function)))

(defun anjsp-check (key)
  "mode change"
  (let ((nowMode anjsp-html-mode)
	(reg (if (= key ?%) "<%\\|%>" "<\\?\\|\\?>")) )
    (save-excursion
      (beginning-of-line)
      (if (bobp) nil
	(if (re-search-backward reg nil t)
	    (if (= key (char-before (+ (point) 1)))
		nil ; html mode
	      (if (= key ?%)
		  (setq nowMode anjsp-java-mode)
		(setq nowMode anjsp-php-mode))))))
    nowMode))

(defun anjsp-set-base-major-mode (mode key)
  "base mode changer."
  (let ((nowMode))
    (cond
     ((eq mode anjsp-java-mode)
      (setq nowMode anjsp-java-mode)
      (cond
       ((fboundp  anjsp-base-java-mode)	(funcall  anjsp-base-java-mode))
       ((fboundp 'jde-mode) (jde-mode))
       ((fboundp 'java-mode) (java-mode))
       (t (c++-mode))))
     ((eq mode anjsp-php-mode)
      (setq nowMode anjsp-php-mode)
      (cond
       ((fboundp  anjsp-base-php-mode) (funcall  anjsp-base-php-mode))
       (t (c++-mode))))
     (t
      (setq nowMode anjsp-html-mode)
      (cond
       ((fboundp  anjsp-base-html-mode)	(funcall  anjsp-base-html-mode))
       ((fboundp 'html-mode) (html-mode))
       (t (c++-mode)))))
    ;;;
    ;;; mode-map
    (setq anjsp-mode-map (copy-keymap (current-local-map)))
    ;;;
    ;;; vs epojava
    (if (eq nowMode 'epojava-mode)
	(progn
	  (substitute-key-definition
	   'epocc-indent-all 'anjsp-not-supprot anjsp-mode-map)
	  (substitute-key-definition
	   'epocc-kandr-style 'anjsp-not-supprot anjsp-mode-map)))
    ;;;
    ;;; 変数が基底メジャーモード呼んだ瞬間に壊れるので復旧
    ;;;
    (make-variable-buffer-local 'anjsp-base-mode)
    (make-variable-buffer-local 'anjsp-key)
    (setq mode-name (format "%c%s" key mode-name)
	  anjsp-base-mode nowMode
	  anjsp-key key))
  ;;;
  (define-key anjsp-mode-map "\t" 'anjsp-tab-key)
  (use-local-map anjsp-mode-map))

(defun anjsp-not-supprot ()
  "not support function"
  (message "This function not support.")
  (sit-for 2))

;  end.
(provide 'anjsp)
;;; epocc.el ends here
