;;; -*- lexical-binding: t -*-
(defun dt-todo (msg)
  (message "todo %s" msg)
  )

(defun dt-open-file ()
  (interactive)

  (ns-open-file-using-panel)
  )

(defun dt-new-untitled-buffer ()
  (interactive)

  (let ((dt-new-buffer (generate-new-buffer "Untitled")))
    (switch-to-buffer dt-new-buffer)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    dt-new-buffer)
  )

(defun dt-close-buffer ()
  (interactive)

  (if (> (count-windows) 1)
      (kill-buffer-and-window)
    (kill-buffer (current-buffer)))
  )

(defun dt-reopen-closed-buffer ()
  (interactive)

  ;; close minibuffer
  (dt-todo "dt-reopen-closed-buffer")
  )

(defun dt-open-next-buffer ()
  (interactive)

  (next-buffer)
  )

(defun dt-open-previous-buffer ()
  (interactive)

  (previous-buffer)
  )


(defun dt-insert-line-above ()
  (interactive)

  (beginning-of-line)
  (open-line 1)
  )

(defun dt-insert-line-below ()
  (interactive)

  (end-of-line)
  (open-line 1)
  (next-line)
  )

(defun dt-expand-line-selection ()
  (interactive)

  (if (not (use-region-p))
      (progn
        (set-mark (line-beginning-position))
        (goto-char (line-beginning-position 2)))

    (let ((start (save-excursion (goto-char (region-beginning))(line-beginning-position)))
          (end (save-excursion (goto-char (region-end))(line-beginning-position 2))))
      (set-mark start)
      (goto-char end)))
  )

(defun dt--selection-expand ()
  (let ((start (save-excursion (goto-char (region-beginning))
                               (line-beginning-position)))

        (end (save-excursion (goto-char (region-end))
                             (if (> (point) (line-beginning-position))
                                 (line-beginning-position 2)
                               (line-beginning-position)))))

    (set-mark start)
    (goto-char end))
  )

(defun dt--line-or-selection-move (number-of-lines)
  (if (region-active-p)
      (let ((text (delete-and-extract-region (region-beginning) (region-end))))
        (forward-line number-of-lines)

        (let ((start (point)))
          (insert text)
          (setq deactivate-mark nil)
          (set-mark start)))

    (progn
      (forward-line 1)
      (transpose-lines number-of-lines)
      (forward-line -1)
      ))
  )

(defun dt-move-line-down ()
  (interactive)

  (if (region-active-p)
      (dt--selection-expand))

  (dt--line-or-selection-move 1)
  )

(defun dt-move-line-up ()
  (interactive)

  (if (region-active-p)
      (dt--selection-expand))

  (dt--line-or-selection-move -1)
  )

(defun dt-copy-line-down ()
  (interactive)

  (let ((pos (if (not (region-active-p))
                 (current-column))))

    (if (region-active-p)
        (dt--selection-expand)
      (dt-expand-line-selection))

    (let ((text (buffer-substring (region-beginning) (region-end)))
          (start (region-end)))
      (insert text)
      (setq deactivate-mark nil)
      (set-mark start))

    (when pos
      (deactivate-mark)
      (forward-line -1)
      (move-to-column pos)))
  )

(defun dt-delete-line ()
  (interactive)

  (if (region-active-p)
      (dt--selection-expand)
    (dt-expand-line-selection))

  (kill-region (region-beginning) (region-end))
  )

(defun dt-join-lines ()
  "Join line below to the end of the current line."
  (interactive)

  (if (region-active-p)
      (let ((number-of-lines (count-lines (region-beginning) (region-end))))
        (when (> number-of-lines 1)
          (goto-char (region-beginning))
          (dotimes (number (1- number-of-lines))
            (delete-indentation 1))))
    (delete-indentation 1))
  )

(defun dt-toggle-line-comment ()
  (interactive)

  (if (region-active-p)
      (progn
        (dt--selection-expand)
        (comment-or-uncomment-region (region-beginning) (region-end))
        (setq deactivate-mark nil))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
  )


(defun dt-indent-line ()
  (interactive)

  ;; turn off electric-indent-mode to avoid any issues with newline.
  (let ((offset (+ (current-indentation)
                   (- c-basic-offset (% (current-indentation) c-basic-offset)))))

    (save-excursion
      (move-to-column 0)
      (when (and (> (point) 1)
                 (not (use-region-p)))
        (forward-line -1)
        (if (> (current-indentation) offset)
            (setq offset (current-indentation))
          ))
      )

    (move-to-column (current-indentation))
    (indent-to offset))
  )

(defun dt-back-indent-line ()
  (interactive)

  (let ((offset (- (current-indentation) (current-column))))
    (delete-region (- (point) (min (max (- c-basic-offset offset) 0) (current-column)))
                   (+ (point) (min offset c-basic-offset))))
  )

(defun dt-back-indent-region ()
  (interactive)

  (if (region-active-p)
      (let ((lines (count-lines (region-beginning) (region-end))))

        (save-excursion
          (goto-char (region-end))
          (when (= (current-column) 0)
            (forward-line -1))
          (dotimes (i lines)
            (dt-back-indent-line)
            (forward-line -1)))
        (setq deactivate-mark nil))

    (dt-back-indent-line))
  )

(defun dt-newline-and-indent ()
  (interactive)
  (newline nil t)
  (indent-according-to-mode)
  )

(provide 'dt-utils)
