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
  (add-to-list 'editorconfig-indentation-alist
	       '(js2-mode js2-basic-offset))
  :hook
  ((js2-mode . editorconfig-mode))
  )

(use-package js2-mode
  :mode "\\.js$"
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
  :config
  (modalka-define-kbd "3" "C-x 1")       ; Delete other windows
  (modalka-define-kbd "SPC 3" "C-x 0")   ; Delete window
  (modalka-define-kbd "4" "C-x 2")       ; Split Horizontally
  (modalka-define-kbd "SPC 4" "C-x 3")   ; Split Vertically
  (modalka-define-kbd "5" "C-d")         ; Delete the following character
  (modalka-define-kbd "SPC 5" "C-x +")   ; Balance windows
  (modalka-define-kbd "SPC 9" "M-$")     ; Check spelling of word under or before the cursor
  (modalka-define-kbd "-" "C-x <left>")  ; Move to previous buffer
  (modalka-define-kbd "=" "C-x <right>") ; Move to next buffer

  (modalka-define-kbd "e" "M-DEL")       ; Backward kill word
  (modalka-define-kbd "r" "M-d")         ; Forward kill word
  (modalka-define-kbd "SPC r" "M-%")     ; Replace some occurrences
  (modalka-define-kbd "t" "C-SPC")       ; Begin Selection
  (modalka-define-kbd "y" "C-_")         ; Undo
  (modalka-define-kbd "u" "M-b")         ; Move backward one word
  (modalka-define-kbd "i" "C-p")         ; Move the cursor to the previous line (upward)
  (modalka-define-kbd "o" "M-f")         ; Move forward one word
  (modalka-define-kbd "SPC p" "C-l")

  (modalka-define-kbd "a" "M-x")         ; Execute extended command
  (modalka-define-kbd "SPC a" "C-x h")   ; Select the whole buffer
  (modalka-define-kbd "s" "C-o")         ; Open line
  (modalka-define-kbd "d" "DEL")         ; Delete character backward
  (modalka-define-kbd "SPC f" "C-x b")   ; Switch to buffer
  (modalka-define-kbd "g" "C-d")         ; Delete the following character
  (modalka-define-kbd "SPC g" "C-k")     ; Kill the rest of the current line
  (modalka-define-kbd "h" "M-{")         ; Move back to previous paragraph beginning
  (modalka-define-kbd "SPC h" "M-<")     ; Move point to the beginning of the buffer
  (modalka-define-kbd "j" "<left>")
  (modalka-define-kbd "k" "C-n")         ; Move the cursor to the next line (downward)
  (modalka-define-kbd "l" "<right>")
  (modalka-define-kbd ";" "M-}")         ; Move forward to next paragraph end

  (modalka-define-kbd "x" "C-w")         ; Cut
  (modalka-define-kbd "c" "M-w")         ; Copy
  (modalka-define-kbd "v" "C-y")         ; Paste (Yank)
  ;; b - toggle letter case
  (modalka-define-kbd "b b" "C-x r b")   ; Open a buffer from bookmarks.
  (modalka-define-kbd "b l" "C-x r l")   ; List bookmarks.
  (modalka-define-kbd "b m" "C-x r m")	 ; Add current buffer to bookmarks
  (modalka-define-kbd "n" "C-s")         ; Incremental search forward
  (modalka-define-kbd "SPC n" "M->")     ; Move point to the end of the buffer
  (modalka-define-kbd "," "C-x o")       ; Switch Window

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
  (modalka-define-kbd ": e" "C-x C-f")   ; Edit file
  (modalka-define-kbd ": r" "C-x C-q")   ; Toggle read-only mode
  (modalka-define-kbd ": s" "C-x C-s")   ; Save current buffer
  (modalka-define-kbd ": x" "C-x C-c")   ; Closes Emacs and asks to save each buffers

  ;; (modalka-define-kbd "W u" "M-u")       ; Make word uppercase
  ;; (modalka-define-kbd "W l" "M-l")       ; Make word lowercase
  ;; (modalka-define-kbd "W c" "M-c")       ; Make word title (Capitalize only first letter)

  ;; (modalka-define-kbd "r b" "C-x r b")   ; Open a buffer from bookmarks.
  ;; (modalka-define-kbd "r l" "C-x r l")   ; List bookmarks.
  ;; (modalka-define-kbd "r m" "C-x r m")	 ; Add current buffer to bookmarks.
  ;; (modalka-define-kbd "r r" "-")         ; Update a buffer when the file is changed on disk
  ;; (modalka-define-kbd "k" "C-k")         ; Delete current line from cursor position
  ;; (modalka-define-kbd ":" "M-<")         ; Move the cursor to the beggining of buffer
  ;; (modalka-define-kbd "\"" "M->")        ; Move to the cursor tor end of buffer

  ;; (modalka-define-kbd "m" "M-m")         ; Jump to first non whitespace in current line
  ;; (modalka-define-kbd "/" "M-;")         ; Comment/ Uncomment Selected Code

  (defun my-modalka-mode-no-git-commit ()
    "Enable ‘modalka-mode’ unless get edit git commit message."
    (unless (string-equal (buffer-name) "COMMIT_EDITMSG")
      (modalka-mode 1)))

  (defun my-modalka-mode-off ()
    "Disable ‘modalka-mode’."
    (interactive)
    (modalka-mode -1))

  :bind
  ((";" . modalka-mode)
   :map
   modalka-mode-map
   ("6" . my-select-block)
   ("7" . my-select-line)
   ("8" . my-extend-selection)
   ("9" . my-select-text-in-quote)

   ("w" . my-shrink-whitespaces)
   ("p" . my-insert-space-before)

   ("f" . my-modalka-mode-off)
   ("SPC k e" . sort-lines)
   ("SPC k w" . sort-numeric-fields)
   ("'" . my-noop)
   ("\\" . my-noop)

   ("m" . my-backward-left-bracket)
   ("." . my-forward-right-bracket)
   ("/" . my-noop)

   ("SPC ;" . my-insert-semicolon)

   ("N" . my-new-empty-buffer)
   ("T" . my-open-last-closed)
   ("W" . my-close-current-buffer)
   )

  :hook
  ((
    emacs-lisp-mode
    help-mode
    js2-mode
    lisp-mode
    prog-mode
    text-mode
    yaml-mode
    ztree-mode
    ) . modalka-mode)
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
