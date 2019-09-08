(progn
  (setq package-archives
        '(
          ("gnu"          . "http://elpa.gnu.org/packages/")
          ("melpa"        . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")))
  (package-initialize)

  (defvar mp-startup-time (current-time))
  (defvar mp-init-time nil)

  (defun mp-compute-startup-time ()
    (setq mp-init-time (float-time (time-subtract (current-time) mp-startup-time)))
    )

  (add-hook 'after-init-hook 'mp-compute-startup-time)

  (defun display-startup-echo-area-message ()
    (message "Initialized in %.3fs" mp-init-time)
    )

  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  (setq ring-bell-function 'ignore)

  ;; Disable backup/autosave files
  (setq make-backup-files        nil)
  (setq auto-save-default        nil)
  (setq auto-save-list-file-name nil)

  ;; Scrolling settings
  (setq scroll-step               1)
  (setq scroll-margin            10)
  (setq scroll-conservatively 10000)

  (defalias 'yes-or-no-p 'y-or-n-p)
  (menu-bar-mode 0)
  (delete-selection-mode 1)
  (column-number-mode 1)

  ;; Coding-system settings
  (set-language-environment 'UTF-8) ; slow one, delay?

  ;; Show-paren-mode settings
  (show-paren-mode t) ; enable expression highlighting between {},[],()

  ;; Electric-modes settings
  (electric-indent-mode -1)

  ;; Delete selection
  (delete-selection-mode t)

  ;; Indent settings
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width          2)
  (setq-default c-basic-offset     2)
  (setq-default standart-indent    2)
  (setq-default lisp-body-indent   2)

  ;; End of file newlines
  (setq require-final-newline    t)
  (setq next-line-add-newlines nil)

  (setq custom-file (concat user-emacs-directory "customize.el"))
  )


(progn
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (require 'core-autocompile)
  (require 'core-ui)
  (require 'core-keys)
  (require 'core-misc)
  )


(progn
  (keys-action-define "eval-last-sexp" nil 'eval-last-sexp)
  (keys-action-define "eval-last-sexp" "es-mode" 'es-nodejs-eval-line)
  (keys-action-define "eval-region" nil 'eval-region)
  (keys-action-define "eval-region" "es-mode" 'es-nodejs-eval-region)

  ;; keymap with space prefix
  (define-prefix-command 'keys-space-map)

  (define-key keys-space-map "\t" 'indent-region)

  (define-key keys-space-map "3" 'delete-window)
  (define-key keys-space-map "4" 'split-window-right)
  (define-key keys-space-map "5" 'balance-windows)
  (define-key keys-space-map "-" 'xah-previous-emacs-buffer)
  (define-key keys-space-map "=" 'xah-next-emacs-buffer)

  (define-key keys-space-map "e" '(lambda () (interactive) (keys-action-call "eval-last-sexp")))
  (define-key keys-space-map "r" '(lambda () (interactive) (keys-action-call "eval-region")))
  (define-key keys-space-map "t" 'beginning-of-buffer)
  (define-key keys-space-map "o" 'workspace-show-project)
  (define-key keys-space-map "p" 'xah-backward-left-bracket)
  (define-key keys-space-map "]" 'xah-forward-right-bracket)

  (define-key keys-space-map "a" 'mark-whole-buffer)
  (define-key keys-space-map "f" 'switch-to-buffer)
  (define-key keys-space-map "g" 'keyboard-quit)
  (define-key keys-space-map "l" 'recenter)

  (define-key keys-space-map "b" 'end-of-buffer)

  ;; command mode keybindings
  (keys-define "1" 'keys-noop)
  (keys-define "2" 'keys-noop)
  (keys-define "3" 'delete-other-windows)
  (keys-define "4" 'split-window-below)
  (keys-define "5" 'keys-noop)
  (keys-define "6" 'xah-select-block)
  (keys-define "7" 'xah-select-current-line)
  (keys-define "8" 'xah-extend-selection)
  (keys-define "9" 'keys-noop)
  (keys-define "0" 'keys-noop)
  (keys-define "-" 'xah-previous-user-buffer)
  (keys-define "=" 'xah-next-user-buffer)

  (keys-define "q" 'keys-noop)
  (keys-define "w" 'xah-shrink-whitespaces)
  (keys-define "e" 'xah-backward-kill-word)
  (keys-define "r" 'xah-kill-word)
  (keys-define "t" 'set-mark-command)
  (keys-define "y" 'undo)
  (keys-define "u" 'keys-noop)
  (keys-define "i" 'keys-noop)
  (keys-define "o" 'backward-paragraph)
  (keys-define "p" 'backward-word)
  (keys-define "[" 'previous-line)
  (keys-define "]" 'forward-word)

  (keys-define "a" 'execute-extended-command)
  (keys-define "s" 'open-line)
  (keys-define "d" 'xah-delete-backward-char-or-bracket-text)
  (keys-define "f" 'keys-insertion-mode-activate)
  (keys-define "g" 'keyboard-quit)
  (keys-define "h" 'keys-noop)
  (keys-define "j" 'keys-noop)
  (keys-define "k" 'keys-noop)
  (keys-define "l" 'forward-paragraph)
  (keys-define ";" 'backward-char)
  (keys-define "'" 'next-line)
  (keys-define "\\" 'forward-char)

  (keys-define "z" 'keys-noop)
  (keys-define "x" 'xah-cut-line-or-region)
  (keys-define "c" 'xah-copy-line-or-region)
  (keys-define "v" 'xah-paste-or-paste-previous)
  (keys-define "b" 'keys-noop)
  (keys-define "n" 'isearch-forward)
  (keys-define "m" 'xah-goto-matching-bracket)
  (keys-define "," 'xah-next-window-or-frame)
  (keys-define "." 'keys-noop)
  (keys-define "/" 'comment-line)

  (keys-define " " 'keys-space-map)

  ;; common keybindings
  (define-key keys-mode-map "§" 'keys-command-mode-activate)

  (define-key keys-mode-map "\C-w" 'xah-close-current-buffer)
  (define-key keys-mode-map "\C-o" 'find-file)
  (define-key keys-mode-map "\C-p" 'xah-beginning-of-line-or-block)
  (define-key keys-mode-map "\C-]" 'xah-end-of-line-or-block)
  (define-key keys-mode-map "\C-s" 'save-buffer)
  (define-key keys-mode-map "\C-n" 'xah-new-empty-buffer)

  (define-key isearch-mode-map [up] 'isearch-ring-retreat)
  (define-key isearch-mode-map [down] 'isearch-ring-advance)
  (define-key isearch-mode-map [left] 'isearch-repeat-backward)
  (define-key isearch-mode-map [right] 'isearch-repeat-forward)

  (define-key minibuffer-local-isearch-map [left] 'isearch-reverse-exit-minibuffer)
  (define-key minibuffer-local-isearch-map [right] 'isearch-forward-exit-minibuffer)

  (global-set-key (kbd "M-_") 'mp-insert-mdash)

  (keys-mode 1)
  )


(progn
  (defun mp-refresh-contents (&rest args)
    (package-refresh-contents)
    (advice-remove 'package-install 'mp-refresh-contents))

  (advice-add 'package-install :before 'mp-refresh-contents)

  (defun mp-install-packages (&rest mp-packages)
    (dolist (mp-package mp-packages)
      (unless (package-installed-p mp-package)
        (require 'package)
        (package-install mp-package)
        ))
    )

  (mp-install-packages 'neotree)

  (require 'core-ido)
  (require 'core-clipboard)
  (require 'core-workspace)
  (autoload 'workspace-show-project "core-neotree" nil t)
  (autoload 'es-mode "core-ecmascript" "Edit EcmaScript files" t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . es-mode))
  )
