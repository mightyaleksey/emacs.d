;;; -*- lexical-binding: t -*-

;; Define the list of external packages that will be loaded.
(setq required-packages
      '(
        company              ; Modular in-buffer completion framework
        doom-themes
        find-file-in-project
        fiplr                ; Find in project
        fireplace
        js2-mode             ; Improved JavaScript editing mode
        neotree
        smart-mode-line      ; A powerful and beautiful mode-line for Emacs
        undo-tree
        use-package          ; Declarative package configuration
        ))

(require 'package)
(require 'bytecomp)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package required-packages)
  (when (and (assq package package-archive-contents)
             (not (package-installed-p package)))
    (package-install package t)))


;; Move customizations outside of "init.el" file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file) (load custom-file))

;; Define path for custom packages and add it to load path,
;; so we can require custom packages by name from this directory.
(defvar mp-package-dir (expand-file-name "mp" user-emacs-directory)
  "My package directory.")

(add-to-list 'load-path mp-package-dir)


(require 'mp-ui)
(require 'mp-hotkeys)
(require 'use-package)

(use-package company
  :config
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 3)

  :hook
  ((emacs-lisp-mode js2-mode lisp-mode) . company-mode)
  )

(use-package find-file-in-project
  :config
  ;; assumption that cwd is a root for project.
  (setq ffip-project-root default-directory)

  :bind
  ("M-p" . find-file-in-project)
  )

(use-package js2-mode
  :mode "\\.js$"
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default js-indent-level 2)
  (setq-default js2-basic-offset 2)
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  )

(use-package linum
  :hook
  ((js2-mode prog-mode) . linum-mode)

  :config
  (setq linum-format "%4d ")
  )

(use-package neotree
  :config
  (setq neo-banner-message nil)
  (setq neo-smart-open t)
  (setq neo-show-hidden-files t)
  (setq neo-show-updir-line nil)
  (setq neo-theme 'ascii)
  (setq neo-window-width 30)

  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root."
    (interactive)
    (let ((project-dir (ffip-project-root))
          (file-name (buffer-file-name)))
      (if (neo-global--window-exists-p)
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))


  :bind
  ("C-b" . neotree-project-dir-toggle)
  )

(use-package smart-mode-line
  :config
  (column-number-mode 1)
  (setq sml/no-confirm-load-theme t)
  (setq sml/numbers-separator ",")
  (setq sml/replacer-regexp-list nil)
  (setq sml/theme 'respectful)
  (sml/setup)
  ;; (custom-theme-set-faces 'smart-mode-line-respectful
  ;;        '(mode-line ((t :background "gray98" :inverse-video nil))))
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)

  :bind
  ("C-y" . undo-tree-redo)
  )
