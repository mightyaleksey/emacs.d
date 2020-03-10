;;; -*- lexical-binding: t -*-

(require 'use-package)
(require 'my-utils)

(use-package company
  :config
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 5)
  :hook
  ((emacs-lisp-mode js2-mode lisp-mode) . company-mode)
  )

(use-package company-tabnine
  :after (company)
  :config
  (add-to-list 'company-backends #'company-tabnine)
  )

(use-package cyphejor
  :init
  (setq
   cyphejor-rules
   '(:upcase
     ("bookmark"    "→")
     ("buffer"      "β")
     ("diff"        "Δ")
     ("dired"       "δ")
     ("emacs"       "ε")
     ("eshell"      "εsh")
     ("inferior"    "i" :prefix)
     ("interaction" "i" :prefix)
     ("interactive" "i" :prefix)
     ("lisp"        "λ" :postfix)
     ("menu"        "▤" :postfix)
     ("mode"        "")
     ("package"     "↓")
     ("python"      "π")
     ("shell"       "sh" :postfix)
     ("text"        "ξ")
     ("wdired"      "↯δ")))
  :config
  (cyphejor-mode 1)
  )

(use-package editorconfig
  :config
  (setq editorconfig-mode-lighter " εc")
  (add-to-list 'editorconfig-indentation-alist
               '(js2-mode js2-basic-offset))
  :hook
  ((js2-mode . editorconfig-mode))
  )

(use-package flycheck
  :hook (js2-mode . flycheck-mode)
  :config
  (setq flycheck-disabled-checkers (quote (javascript-jshint)))
  (setq flycheck-idle-buffer-switch-delay 0.6)
  (setq flycheck-idle-change-delay 0.6)
  (setq flycheck-mode-line-prefix "ƒc")

  (defun my-setup-executable ()
    "Setup executables for flycheck base on the current mode."
    (let ((standard-path (my-locate-js-executable "standard"))
          (eslint-path (my-locate-js-executable "eslint")))

      (when standard-path
        (setq-local flycheck-javascript-standard-executable standard-path))

      (when eslint-path
        (setq-local flycheck-javascript-eslint-executable eslint-path))
      ))

  (add-hook 'flycheck-mode-hook 'my-setup-executable)
  )

(use-package key-chord
  :after (modalka)
  :init
  (key-chord-mode 1)
  :config
  (key-chord-define-global "jf" 'modalka-mode)
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

(use-package ivy
  :init
  (setq-default ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  )

(use-package modalka
  :after (my-utils)
  :init
  (setq-default modalka-cursor-type 'box)
  (modalka-global-mode 1)
  :config
  (modalka-define-kbd "n" "C-s") ; Incremental search forward

  (modalka-define-kbd "b b" "C-x r b")   ; Open a buffer from bookmarks.
  (modalka-define-kbd "b l" "C-x r l")   ; List bookmarks.
  (modalka-define-kbd "b m" "C-x r m")	 ; Add current buffer to bookmarks
  (modalka-define-kbd "SPC n" "M->")     ; Move point to the end of the buffer

  (modalka-define-kbd "U" "C-u")         ; Universal argument command

  (modalka-define-kbd "G c" "M-g c")     ; Go to character position
  (modalka-define-kbd "G g" "M-g g")     ; Go to line number
  (modalka-define-kbd "H ?" "C-h ?")     ; Show all commands to get help on Emacs.
  (modalka-define-kbd "H a" "C-h a")     ; Command Apropos
  (modalka-define-kbd "H b" "C-h b")     ; Describe key bindings for current buffer
  (modalka-define-kbd "H f" "C-h f")     ; Describe function
  (modalka-define-kbd "H k" "C-h k")     ; Describe a particular key binding
  (modalka-define-kbd "H m" "C-h m")     ; Describe mode
  (modalka-define-kbd "H s" "C-h s")     ; Describe syntax
  (modalka-define-kbd "H v" "C-h v")     ; Describe variable

  (defun my-modalka-mode-no-git-commit ()
    "Enable ‘modalka-mode’ unless get edit git commit message."
    (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
      (modalka-mode 1)))

  (defun my-modalka-mode-off ()
    "Disable ‘modalka-mode’."
    (interactive)
    (modalka-mode -1))

  :bind
  (
   :map
   modalka-mode-map
   ("3" . delete-other-windows)          ; Delete Other Windows / C-x 1
   ("4" . split-window-below)            ; Split Horizontally / C-x 2
   ("5" . delete-char)                   ; Delete the following character / C-d
   ("6" . my-select-block)
   ("7" . my-select-line)
   ("8" . my-extend-selection)
   ("9" . my-select-text-in-quote)
   ("-" . previous-buffer)               ; Move to previous buffer / C-x <left>
   ("=" . right-char)                    ; Move to next buffer / C-x <right>

   ("w" . my-shrink-whitespaces)
   ("p" . my-insert-space-before)
   ("e" . backward-kill-word)            ; Backward kill word / M-DEL
   ("r" . kill-word)                     ; Forward kill word / M-d
   ("t" . set-mark-command)              ; Begin Selection / C-SPC
   ("y" . undo)                          ; Undo / C-_
   ("u" . backward-word)                 ; Move backward one word / M-b
   ("i" . previous-line)                 ; Move the cursor to the previous line (upward) / C-p
   ("o" . forward-word)                  ; Move forward one word / M-f

   ("a" . execute-extended-command)      ; Execute extended command / M-x
   ("s" . open-line)                     ; Open line / C-o
   ("d" . backward-delete-char-untabify) ; Delete character backward / DEL
   ("f" . my-modalka-mode-off)
   ("g" . delete-char)                   ; Delete the following character / C-d
   ("h" . backward-paragraph)            ; Move back to previous paragraph beginning / M-{
   ("j" . left-char)
   ("k" . next-line)                     ; Move the cursor to the next line (downward) / C-n
   ("l" . right-char)
   (";" . forward-paragraph)             ; Move forward to next paragraph end / M-}
   ("'" . my-noop)
   ("\\" . my-noop)

   ("x" . kill-region)                   ; Cut / C-w
   ("c" . kill-ring-save)                ; Copy / M-w
   ("v" . yank)                          ; Paste (Yank) / C-y
   ;; b - toggle letter case
   ;; ("n" . search-forward???)                ; Incremental search forward / C-s
   ("," . other-window)                  ; Switch Window / C-x o
   ("m" . my-backward-left-bracket)
   ("." . my-forward-right-bracket)
   ("/" . my-noop)

   ("SPC 3" . delete-window)             ; Delete window / C-x 0
   ("SPC 4" . split-window-right)        ; Split Vertically / C-x 3
   ("SPC 5" . balance-windows)           ; Balance windows / C-x +
   ("SPC 9" . ispell-word)               ; Check spelling of word under or before the cursor / M-$

   ("SPC r" . query-replace)             ; Replace some occurrences / M-%
   ("SPC p" . recenter-top-bottom)       ; C-l

   ("SPC a" . mark-whole-buffer)         ; Select the whole buffer / C-x h
   ("SPC f" . ivy-switch-buffer)         ; Switch to buffer / C-x b
   ("SPC g" . kill-line)                 ; Kill the rest of the current line / C-k
   ("SPC h" . beginning-of-buffer)       ; Move point to the beginning of the buffer / M-<

   ("SPC k e" . sort-lines)
   ("SPC k w" . sort-numeric-fields)

   ("W" . my-close-current-buffer)
   ("R" . read-only-mode) ; Toggle read-only mode
   ("T" . my-open-last-closed)
   ("O" . find-file) ; Edit file

   ("S" . save-buffer) ; Save current buffer

   ("X" . save-buffers-kill-terminal) ; Closes Emacs and asks to save each buffers
   ("N" . my-new-empty-buffer)
   )
  )

(use-package smart-mode-line
  :config
  (column-number-mode 1)
  (setq sml/no-confirm-load-theme t)
  (add-to-list 'rm-blacklist " company")
  (add-to-list 'rm-blacklist " ivy")
  (setq sml/numbers-separator ",")
  (setq sml/replacer-regexp-list nil)
  (setq sml/theme 'respectful)
  (sml/setup)
  )

(use-package smartparens
  :config
  (smartparens-global-mode 1)
  )

(use-package swiper
  :bind
  ("C-s" . swiper)
  )

(use-package time
  :demand
  :init
  (setq display-time-24hr-format t)
  (setq display-time-default-load-average nil)
  :config
  (display-time-mode 1)
  )

(provide 'my-packages)
