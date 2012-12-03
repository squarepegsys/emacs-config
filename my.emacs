(add-to-list 'load-path (expand-file-name "~/.emacs.d/" load-path))
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/icicles"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet" load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/django-mode" load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/etags-update" load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scala-mode" load-path))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/groovy-emacs-mode" load-path))
;(normal-erase-is-backspace-mode 1)

(setenv "PATH" (concat "/Users/mhostetler/bin:/usr/local/bin:"
                (getenv "PATH")))


(require 'ido)
(require 'yasnippet)

(yas/global-mode 1)
(yas/initialize)

; pretty colors
(require 'color-theme)
(color-theme-initialize)
(color-theme-kingsajz)

;;; add these lines if you like color-based syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; to setup tabs
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)


(load-file "/Users/mhostetler/.emacs.d/emacs-for-python/epy-init.el")  
;(epy-setup-checker "~/bin/pyflakes %f")
(epy-setup-ipython)

;(require 'highlight-indentation)
;(add-hook 'python-mode-hook 'highlight-indentation)
(desktop-save-mode 1)

(require 'django-html-mode)
(require 'django-mode)
;(yas/load-directory "~/.emacs.d/django-mode/snippets")
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;; un-tab any python files
 (defun python-mode-untabify ()
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (if (search-forward "\t" nil t)
          (untabify (1- (point)) (point-max))))
    nil)

  (add-hook 'python-mode-hook 
            '(lambda ()
               (make-local-variable 'write-contents-hooks)
               (add-hook 'write-contents-hooks 'python-mode-untabify)))

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(require 'groovy-mode)
;(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;;; groovy mode seems to overwrite the tab
(add-hook 'groovy-mode-hook
	  #'(lambda () (local-set-key [tab] 'yas/expand))
)



(server-start)

;; Text Options
(setq-default fill-column 70)
;(setq-default fill-prefix "    ")
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Display column number
(column-number-mode t)

;; Display date/time
(setq display-time-day-and-date t)
(display-time)


;; Map M-g for goto-line (like with XEmacs)
(global-set-key "\M-g" 'goto-line)    

;;;;;;;;;;;;;;;;;
;; from http://opal.cabochon.com/~stevey/blog-rants/effective-emacs.html
;;;;;;;;;;;;;;;;;
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


;; Convert tabs to spaces 
(setq indent-tabs-mode nil)

;; Replace "yes or no" with y or n
(defun yes-or-no-p (arg)
  "An alias for y-or-n-p, because I hate having to type 'yes' or 'no'."
  (y-or-n-p arg))


(setq-default abbrev-mode t)
(load "~/.emacs.d/mh-abbrevs.el")

;; set dictionary
(setq-default ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "american")
(setq flyspell-default-dictionary "american")

(require 'tramp)
(add-to-list 'backup-directory-alist
    (cons "." "~/.emacs.d/backups/"))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory "~/.emacs.d/backups/")


(require 'scss-mode)

(require 'etags-update)

(setq yas/my-directory (expand-file-name "~/.emacs.d/yasnippet/snippets"))
(yas/load-directory yas/my-directory)

(require 'scala-mode-auto)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

(put 'upcase-region 'disabled nil)

(set-face-attribute 'default nil
                :family "Inconsolata" :height 145 :weight 'normal)

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t) 
(setq auto-mode-alist (cons '("\\.text" . markdown-mode) auto-mode-alist))


(auto-insert-mode)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-insert-directory "~/.emacs.d/autoinsert/")
 '(ido-mode (quote both) nil (ido))
 '(scss-compile-at-save nil))

(define-auto-insert "\.sbt" "template.sbt")
(winner-mode 1)


(add-to-list 'load-path "/path/to/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

 (require 'eshell)
 (require 'em-smart)
 (setq eshell-where-to-jump 'begin)
 (setq eshell-review-quick-commands nil)
 (setq eshell-smart-space-goes-to-end t)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
