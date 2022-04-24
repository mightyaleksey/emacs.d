;; -*- lexical-binding: t -*-

;; adds path to teatime emacs modules
(add-to-list 'load-path (file-name-directory
                         (or load-file-name buffer-file-name)))

;; no startup  screen
(setq inhibit-startup-screen t)

;; no startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; no message in scratch buffer
(setq initial-scratch-message nil)

(setq initial-buffer-choice nil)

;; no scroll bars
(if (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

;; no menu and toolbar
(menu-bar-mode 0)
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)

;; backups
(setq-default auto-save-default nil)
(setq-default delete-old-versions t)
(setq-default make-backup-files t)
(setq-default backup-directory-alist '(("." . "~/.local/share/emacs/backups/")))

;; editing
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default global-visual-line-mode t)
(setq-default require-final-newline t)
(setq-default truncate-lines t)

;; fix default-directory = "/"
(when (equal default-directory "/")
  (setq default-directory "~/.emacs.d/")
  (setq command-line-default-directory "~/.emacs.d/"))

;; to fix key sequences make sure to that
;; terminal's keyboard profile has following sequences:
;;
;; ctrl+up    - \033[1;5A
;; ctrl+down  - \033[1;5B
;; shift+up   - \033[1;2A
;; shift+down - \033[1;2B
;;
;; see https://en.wikipedia.org/wiki/ANSI_escape_code
;; and https://en.wikipedia.org/wiki/ASCII
(when (equal (getenv "TERM") "xterm-256color")
  ;; translate esc up to alt+up
  (define-key input-decode-map "\e\eOA" (kbd "<M-up>"))
  ;; translate esc down to alt+down
  (define-key input-decode-map "\e\eOB" (kbd "<M-down>")))

;; load core modules
(require 'teatime-theme)
(require 'teatime-workspace)
(require 'teatime-sidebar)
(require 'teatime-bindings)

(teatime-workspace-mode 1)

;; use "emacs -nw -a ." to add default workspace folder
(defun teatime-workspace-add-folder-command-handler (args)
  (when (= (length command-line-args-left) 0)
    (user-error "Option '-a' requires an argument"))

  (teatime-workspace-add-folder (expand-file-name (car command-line-args-left)))
  (setq command-line-args-left (cdr command-line-args-left)))

(add-to-list 'command-switch-alist '("-a" . teatime-workspace-add-folder-command-handler))

;; set path where to save customizations
(setq custom-file "~/.local/share/emacs/custom.el")
(if (file-exists-p custom-file) (load custom-file))

(provide 'teatime)
