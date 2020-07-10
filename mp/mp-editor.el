;;; -*- lexical-binding: t -*-

;; Set of helper functions for text editing.

;; Backup / autosave files.
(setq auto-save-default nil)
(setq make-backup-files t)
(setq backup-directory-alist (list (cons "." (expand-file-name "backups" user-emacs-directory))))
(setq delete-old-versions t)

;; Fix whitespaces and ensure newline in the end of file.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default require-final-newline t)
(add-hook 'before-save-hook 'whitespace-cleanup)

(delete-selection-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(defun mp-insert-line-after ()
  "Insert line after."
  (interactive)
  (end-of-line)
  (open-line 1)
  (next-line))

(defun mp-insert-line-before ()
  "Insert line before."
  (interactive)
  (beginning-of-line)
  (open-line 1))


(defun mp-move--expand-region ()
  (let ((start (save-excursion (goto-char (region-beginning))
                               (line-beginning-position)))
        (end (save-excursion (goto-char (region-end))
                             (line-end-position))))
    (set-mark start)
    (goto-char end)))

(defun mp-move--text-region (number-of-lines)
  (let ((text (delete-and-extract-region (region-beginning) (1+ (region-end)))))
    (forward-line number-of-lines)
    (let ((start (point)))
      (insert text)
      (setq deactivate-mark nil)
      (set-mark start)
      (forward-line -1)
      (end-of-line))))

(defun mp-move-line-or-selection-up ()
  "Move up the current line or selection."
  (interactive)
  (if (region-active-p)
      (progn
        (mp-move--expand-region)
        (mp-move--text-region -1))
    (progn
      (transpose-lines 1)
      (forward-line -2))))

(defun mp-move-line-or-selection-down ()
  "Move down the current line or selection."
  (interactive)
  (if (region-active-p)
      (progn
        (mp-move--expand-region)
        (mp-move--text-region 1)
        )
    (progn
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1))))

(defun mp-select-line ()
  "Select current line. Repeat to select next lines."
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun mp-duplicate-line ()
  "Duplicate current line or selection."
  (interactive)
  (if (region-active-p)
      (let ((start (if (> (point) (mark)) (region-end)))
            (text (buffer-substring (region-beginning) (region-end))))
        (insert text)
        (setq deactivate-mark nil)
        (when start
          (set-mark start)))
    (let ((column (current-column))
            (text (buffer-substring (line-beginning-position) (line-end-position))))
        (mp-insert-line-after)
        (insert text)
        (move-to-column column))))

(defun mp-jump-to-matching-parentheses ()
  "Jump to closing parentheses. Repeat to jump to opening parentheses."
  (interactive)
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (re-search-forward "\\s)") (backward-char 1))))

(defun mp-join-line-below ()
  "Join line below to the end of the current line."
  (interactive)
  (if (region-active-p)
      (let ((number-of-lines (count-lines (region-beginning) (region-end))))
        (when (> number-of-lines 1)
          (goto-char (region-beginning))
          (dotimes (number (1- number-of-lines))
            (delete-indentation 1))))
    (delete-indentation 1)))

(defun mp-comment-line ()
  "Comment or un-comment current line or selection."
  (interactive)
  (if (region-active-p)
      (progn
        (mp-move--expand-region)
        (comment-or-uncomment-region (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))


(provide 'mp-editor)
