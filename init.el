;;; -*- lexical-binding: t -*-

;; notes on language syntax
;;  'my-variable or (quote my-variable) — refer to variable
;;   my-variable — refer to variable value

;; open empty buffer on start
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; hack the world\n\n")

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; todo update path for buffer, since emacs shows it in title, probably can omit it in mode-line
;; todo remove auto formatting for particular modes
;; todo fix extra indent after ( in sql-mode

;; hide horizontal menu
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; M-x customize-face — to see available faces and props
;; M-x list-colors-display — to see available colors
(set-face-background 'cursor "gray21")
(set-face-background 'fringe "gray100")
(set-face-background 'mode-line "gray21")
(set-face-foreground 'mode-line "gray")
(set-face-background 'mode-line-inactive "dim gray")
(set-face-foreground 'mode-line-inactive "gray89")
(set-face-attribute 'mode-line-buffer-id nil :weight 'bold)
(set-face-foreground 'mode-line-buffer-id "gray89")

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
(set-face-attribute 'default nil :family "Menlo" :height 140)
;; (setq-default line-spacing 0.3) ;; todo how to make it work correctly?

(defun dt-mode-line-space-between (left right)
  (let ((available-width (- (window-total-width)
                            (+ (length (format-mode-line left))
                               (length (format-mode-line right))))))

    (list left (apply 'concat (make-list available-width " ")) right))
  )

;; show UTF-8 instead of U
;; coding-system-alist - available coding system names.
(defvar dt-mode-line-encoding
  '(:eval (upcase
           (replace-regexp-in-string "-\\(dos\\|mac\\|unix\\)" ""
                                     (symbol-name buffer-file-coding-system)))))
(put 'dt-mode-line-encoding 'risky-local-variable t)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html#Mode-Line-Variables
;; https://github.com/kgaipal/emacs/blob/master/lisp/mode-line-customization.el
(setq-default mode-line-format
              '((:eval (dt-mode-line-space-between
                        (format-mode-line (list
                                           " "
                                           (propertize "%b" 'face 'mode-line-buffer-id)))
                        (format-mode-line (list
                                           "Ln %l, Col %c"
                                           "   "
                                           'dt-mode-line-encoding
                                           "   "
                                           'mode-name " " 'minor-mode-alist
                                           " "))))))

;; show better names for major and minor modes in mode line
;; by updating mode-name and minor-mode-alist values
;; https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar dt-mode-line-prettier-alist
  `((company-mode . "α")
    (eldoc-mode . "")
    (yas-minor-mode . "υ")
    ;; major modes
    (css-mode . "CSS")
    (emacs-lisp-mode . "λ")
    (lisp-interaction-mode . "λ")
    (sql-mode . "SQL"))
  "List with better names for major and minor modes.")

(defun dt-prettify-mode-line ()
  (interactive)
  (dolist (item dt-mode-line-prettier-alist)
    (let ((mode (car item))
          (pretty-mode-name (cdr item))
          (minor-mode))
      (setq minor-mode (cdr (assq mode minor-mode-alist)))

      (if minor-mode
          (setcar minor-mode pretty-mode-name))

      (if (eq mode major-mode)
          (setq mode-name pretty-mode-name))
      ))
  )

(add-hook 'after-change-major-mode-hook 'dt-prettify-mode-line)

;; set default encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)

;; backup / autosave files.
(setq auto-save-default nil)
(setq delete-old-versions t)
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.local/share/emacs/backups/")))

(delete-selection-mode 1)
(electric-indent-mode -1)
(show-paren-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default sql-use-indent-support nil)
(setq-default js-offset-level 2)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)

;; save selection after indent-region was called
;; and if custom indent-line-function used
(defun dt-indent-region (start end &optional column)
  (when (and (eq indent-line-function 'dt-indent-line)
             (use-region-p))
    (setq deactivate-mark nil)))

(advice-add 'indent-region :after 'dt-indent-region)

(setq-default global-visual-line-mode t)
(setq-default require-final-newline t)
(setq-default truncate-lines t)
(add-hook 'before-save-hook 'whitespace-cleanup)

(defun dt-reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  )

(defvar dt-local-dir (expand-file-name "local" user-emacs-directory))
(add-to-list 'load-path dt-local-dir)
(require 'dt-utils)
(require 'dt-keys)

(progn
  (require 'package)
  (setq dt-required-packages '(use-package company company-tabnine fireplace multiple-cursors yasnippet))
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package dt-required-packages)
    (when (and (assq package package-archive-contents)
               (not (package-installed-p package)))
      (package-install package t)))
  )

;; todo set folder for .mc-lists.el, custom.el and add them to .gitignore

(require 'use-package)
;; :init to run the code before package is loaded
;; :config to run the code after package is loaded

(use-package company
  :config
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  ;; (setq company-show-numbers t)
  (setq company-tooltip-limit 5)

  :hook
  ((emacs-lisp-mode html-mode lisp-mode sql-mode) . company-mode)
  )

(use-package company-tabnine
  :after (company yasnippet)
  :config
  (setq company-backends '((company-yasnippet company-tabnine)))
  )

(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-L" . mc/edit-ends-of-lines))
  ;; :config (
  ;;   (append 'mc/cmds-to-run-for-all '(dt-insert-line-above dt-insert-line-below))
  ;;   (append 'mc/cmds-to-run-once '(dt-new-untitled-buffer dt-close-buffer dt-reopen-closed-buffer))
  ;; )
  )

(use-package sql
  :config
  (setq indent-line-function 'dt-indent-line)
  )

(use-package yasnippet
  :after (company)
  :config
  (setq yas-indent-line 'fixed)
  (yas-reload-all)

  :hook
  (company-mode . yas-minor-mode)
  )

(setq custom-file "~/.local/share/emacs/custom.el")
(if (file-exists-p custom-file) (load custom-file))
