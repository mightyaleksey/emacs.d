;; -*- lexical-binding: t -*-

;; cmd+q                           - quit

;;  = file
;; cmd+n           [ ctrl+n ]      - new untitled file
;; cmd+s           [ ctrl+s ]      - save file
;; cmd+w           [ ctrl+w ]      - close window

;;  = edit
;; cmd+z           [ ctrl+z ]      - undo

;; cmd+x           [ ctrl+x ]      - cut
;; cmd+c           [ ctrl+c ]      - copy
;; cmd+v           [ ctrl+v ]      - paste

;; cmd+f           [ ctrl+f ]      - find

;; cmd+enter                       - insert line below
;; shift+cmd+enter                 - insert line above
;; cmd+j                           - join lines

;;  = selection
;; cmd+a           [ alt+a ]       - select all
;; cmd+l           [ alt+l ]       - expand line selection

;; alt+up                          - move line up
;; alt+down                        - move line down
;; cmd+d           [ alt+d ]       - copy line down

;;  = view
;; shift+cmd+p     [ shift+alt+p ] - show all commands

;;  = go
;; cmd+p           [ alt+p ]       - go to file

;; shift+cmd+]     [ shift+alt+] ] - open next editor
;; shift+cmd+[     [ shift+alt+[ ] - open previous editor

;;  = tbd
;; cmd+k cmd+b     - toggle side bar visibility
;; shift+cmd+t     - reopen closed editor

(require 'teatime-fsearch)
(require 'teatime-utils)

(defgroup teatime-bindings nil ""
  :group 'teatime)

(defcustom teatime-bindings-jump-lines 7
  "Number of lines to jump."
  :group 'teatime-bindings)

;;
;; g e n e r i c   h e l p e r s
;;

(defun teatime-sort-lines ()
  "Sort lines in ascending order and save selections."
  (interactive)
  (let ((cursor (point))
        (pos (teatime--selected-lines))
        (reg (teatime--region-bounds)))

    (when reg
      (atomic-change-group
        (sort-lines nil (car pos) (cdr pos))

        (setq deactivate-mark nil)
        (goto-char (cdr reg))
        (set-mark (car reg))))))

;;
;; b i n d i n g s
;;

(defun teatime-bindings--move-lines-is-out-of-range (number-of-lines cursor pos)
  (if (< number-of-lines 0)
      (= (teatime--count-lines-before-position
          (min (car pos) (cdr pos))) 0)
    (= (teatime--count-lines-after-position
	      (max (car pos) (cdr pos) cursor)) 0)))

(defun teatime-bindings--move-lines (number-of-lines)
  (let ((cursor (point))
        (pos (teatime--selected-lines))
        (reg (teatime--region-bounds))
        (has-newline t)
        (str nil)
        (tmp -1))

    (unless (teatime-bindings--move-lines-is-out-of-range
	     number-of-lines
	     cursor
	     pos)

      ;; generic approach to move lines:
      ;; - above \n lines \n below \n content
      ;;   copy -> |_____| |         |
      ;;    cut -> |_______|         |
      ;;              move cursor -> |
      ;;
      ;;    add "buf \n" -> |-----|
      ;; - above \n below \n buf \n content

      (atomic-change-group
        (if (= (cdr pos) (point-max)) (setq has-newline nil))

        (setq str (buffer-substring (car pos) (cdr pos)))
        (goto-char (cdr pos))
        (if (and (< number-of-lines 0) (not has-newline))
            (progn
              (delete-region (1- (car pos)) (cdr pos))
              (beginning-of-line))
          (progn
            (delete-region (car pos) (min (1+ (cdr pos)) (point-max)))
            (if (and (= (line-end-position) (point-max))
                     (> number-of-lines 0))
              (setq has-newline nil))
            (forward-line number-of-lines)))

        (if (and (> number-of-lines 0) (not has-newline))
            (insert "\n" str)
          (insert str "\n"))

        ;; compute new cursor position
        (if (and (> number-of-lines 0) (not has-newline)) (setq tmp 0))
        (setq tmp (+ (point) (- (cdr pos)) cursor tmp))

        (if reg
            (progn
              (setq deactivate-mark nil)
              (goto-char tmp)
              (set-mark (+ tmp (- (cdr reg)) (car reg))))
          (goto-char tmp))))))

(defun teatime-bindings-copy-line-down ()
  (interactive)

  (let ((cursor (point))
        (pos (teatime--selected-lines))
        (reg (teatime--region-bounds))
        (str nil)
        (tmp nil))

    (atomic-change-group
      (setq str (buffer-substring (car pos) (cdr pos)))
      (goto-char (cdr pos))
      (insert "\n" str)

      (setq tmp (+ (point) (- (cdr pos)) cursor))

      (if reg
          (progn
            (setq deactivate-mark nil)
            (goto-char tmp)
            (set-mark (+ tmp (- (cdr reg)) (car reg))))
        (goto-char tmp)))))

(defun teatime-bindings-expand-line-selection ()
  "Expand selection to the full line."
  (interactive)

  (if (region-active-p)
      (let ((start (save-excursion
                     (goto-char (region-beginning))
                     (line-beginning-position)))
            (end (save-excursion
                   (goto-char (region-end))
                   (line-beginning-position 2))))
            (set-mark start)
            (goto-char end))

    (progn
      (set-mark (line-beginning-position))
      (goto-char (line-beginning-position 2)))))

(defun teatime-bindings-insert-line-below ()
  "Insert empty line below the current line."
  (interactive)

  (end-of-line)
  (open-line 1)
  (next-line))

(defun teatime-bindings-insert-line-above ()
  "Insert empty line above the current line."
  (interactive)

  (beginning-of-line)
  (open-line 1))

(defun teatime-bindings-join-lines ()
  (interactive)

  (if (region-active-p)
      (let ((number-of-lines (count-lines (region-beginning) (region-end))))
        (when (> number-of-lines 1)
          (goto-char (region-beginning))
          (dotimes (number (1- number-of-lines))
            (delete-indentation 1))))

    (delete-indentation 1)))

(defun teatime-bindings-move-line-up ()
  "Move selected lines above previous line."
  (interactive)
  (teatime-bindings--move-lines -1))

(defun teatime-bindings-move-line-down ()
  "Move selected lines below next line."
  (interactive)
  (teatime-bindings--move-lines 1))

(defun teatime-bindings-jump-up ()
  "Jump multiple lines forward. Number of lines
to jump is defined by 'teatime-bindings-jump-lines'.

Use M-X customize-variable teatime-bindings-jump-lines
to adjust the value."
  (interactive)

  (let ((column (current-column))
        (lines (teatime--count-lines-before-position (point))))

    (if (>= lines teatime-bindings-jump-lines)
        (forward-line (- teatime-bindings-jump-lines))
      (progn
        (goto-char (point-max))
        (forward-line (- lines teatime-bindings-jump-lines -1))))

    (goto-char (min
		(+ (line-beginning-position) column)
		(line-end-position)))))

(defun teatime-bindings-jump-down ()
  "Jump multiple lines backward. Number of lines
to jump is defined by 'teatime-bindings-jump-lines'.

Use M-X customize-variable teatime-bindings-jump-lines
to adjust the value."
  (interactive)

  (let ((column (current-column))
        (lines (teatime--count-lines-after-position (point))))

    (if (>= lines teatime-bindings-jump-lines)
        (forward-line teatime-bindings-jump-lines)
      (progn
        (goto-char (point-min))
        (forward-line (- teatime-bindings-jump-lines lines 1))))

    (goto-char (min
		(+ (line-beginning-position) column)
		(line-end-position)))))

;;
;; h o t k e y s
;;

(if (display-graphic-p)
    (progn
      ;;  = file
      (global-set-key (kbd "s-n") 'teatime-new-untitled-file)
      (global-set-key (kbd "s-s") 'teatime-save-file)
      (global-set-key (kbd "s-w") 'teatime-close-window)

      ;;  = edit
      (global-set-key (kbd "s-z") 'undo)

      (global-set-key (kbd "s-x") 'kill-region)
      (global-set-key (kbd "s-c") 'kill-ring-save)
      (global-set-key (kbd "s-v") 'yank)

      (global-set-key (kbd "s-f") 'isearch-forward)

      (global-set-key (kbd "<s-return>") 'teatime-bindings-insert-line-below)
      (global-set-key (kbd "<S-s-return>") 'teatime-bindings-insert-line-above)
      (global-set-key (kbd "s-j") 'teatime-bindings-join-lines)

      ;;  = selection
      (global-set-key (kbd "s-a") 'mark-whole-buffer)
      (global-set-key (kbd "s-l") 'teatime-bindings-expand-line-selection)

      (global-set-key (kbd "<M-up>") 'teatime-bindings-move-line-up)
      (global-set-key (kbd "<M-down>") 'teatime-bindings-move-line-down)
      (global-set-key (kbd "s-D") ' teatime-bindings-copy-line-down)

      ;;  = view
      (global-set-key (kbd "s-P") 'execute-extended-command)

      ;;  = go
      (global-set-key (kbd "s-p") 'teatime-fsearch)

      (global-set-key (kbd "s-{") 'previous-buffer)
      (global-set-key (kbd "s-}") 'next-buffer)

      (global-set-key (kbd "<C-up>") 'teatime-bindings-jump-up)
      (global-set-key (kbd "<C-down>") 'teatime-bindings-jump-down)

      ;;  = tbd
      (global-set-key (kbd "s-b") 'teatime-sidebar)

      ;; customize find hotkeys
      (define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
      (define-key isearch-mode-map (kbd "<S-return>") 'isearch-repeat-backward)
      (define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)
      (define-key isearch-mode-map (kbd "ESC") 'isearch-exit))

  (progn
    (global-set-key (kbd "M-q") 'save-buffers-kill-terminal)

    ;;  = file
    (global-set-key (kbd "C-n") 'teatime-new-untitled-file)
    (global-set-key (kbd "C-s") 'teatime-save-file)
    (global-set-key (kbd "C-w") 'teatime-close-window)

    ;;  = edit
    (global-set-key (kbd "C-z") 'undo)

    ;; (global-set-key (kbd "C-x") 'kill-region)
    (global-set-key (kbd "C-c") 'kill-ring-save)
    (global-set-key (kbd "C-v") 'yank)

    (global-set-key (kbd "C-f") 'isearch-forward)

    (global-set-key (kbd "M-RET") 'teatime-bindings-insert-line-below)
    (global-set-key (kbd "M-j") 'teatime-bindings-join-lines)

    ;;  = selection
    (global-set-key (kbd "M-a") 'mark-whole-buffer)
    (global-set-key (kbd "M-l") 'teatime-bindings-expand-line-selection)

    (global-set-key (kbd "<M-up>") 'teatime-bindings-move-line-up)
    (global-set-key (kbd "<M-down>") 'teatime-bindings-move-line-down)
    (global-set-key (kbd "M-D") ' teatime-bindings-copy-line-down)

    ;;  = view
    (global-set-key (kbd "M-P") 'execute-extended-command)

    ;;  = go
    (global-set-key (kbd "M-p") 'teatime-fsearch)

    (global-set-key (kbd "M-{") 'previous-buffer)
    (global-set-key (kbd "M-}") 'next-buffer)

    (global-set-key (kbd "<C-up>") 'teatime-bindings-jump-up)
    (global-set-key (kbd "<C-down>") 'teatime-bindings-jump-down)

    ;;  = tbd
    (global-set-key (kbd "C-b") 'teatime-sidebar)

    ;; customize find hotkeys
    (define-key isearch-mode-map (kbd "RET") 'isearch-repeat-forward)
    (define-key isearch-mode-map (kbd "M-RET") 'isearch-repeat-backward)))

(provide 'teatime-bindings)
