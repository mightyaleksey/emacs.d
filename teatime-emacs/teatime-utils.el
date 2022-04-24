;; -*- lexical-binding: t -*-

;;
;; e d i t i n g
;;

(defun teatime--newline-at-eof ()
  "Checks if there is a newline at the end of the
buffer. Returns t or nil otherwise."
  (save-excursion
    (goto-char (point-max))
    (= (current-column) 0)))


(defun teatime--count-lines-after-position (position)
  (let ((lines (count-lines position (point-max))))

    (if (and (> lines 0)
             (not (teatime--newline-at-eof)))
        (1- lines)
      lines)))

(defun teatime--count-lines-before-position (position)
  (count-lines position (point-min)))

(defun teatime--region-bounds ()
  "Returns cons cell with region boundaries if
there is an active region, otherwise nil.

Second value contains cursor position. So if
region beginning has same position as cursor than
first value will be greater than the second one.

Use (car bounds) and (cdr bounds) to access
values."
  (when (region-active-p)
    (if (= (point) (region-end))
        (cons (region-beginning) (region-end))
      (cons (region-end) (region-beginning)))))

(defun teatime--selected-lines ()
  "Returns cons cell with boundaries of selected
lines. Boundaries contain beginning of the first
line and end of the last line.

Use (car lines) and (cdr lines) to access values."
  (if (region-active-p)
      (let ((beginning (save-excursion
                         (goto-char (region-beginning))
                         (line-beginning-position)))
            (end (save-excursion
                   (goto-char (region-end))
                   (line-end-position)))
            (reg (teatime--region-bounds)))

        ;; if cursor at the beginning of the new line,
        ;; than do not capture this line.
        (if (and (= (current-column) 0)
                 (> (cdr reg) (car reg)))
            (setq end (save-excursion
			(goto-char (region-end))
			(forward-line -1)
			(line-end-position))))

        (cons beginning end))

    (cons
     (line-beginning-position)
     (line-end-position))))

;;
;; b u f f e r s   &   w i n d o w s
;;

(defvar teatime--service-buffer-names '("*Help*" "*Messages*")
  "List of read only buffer names that do not
respesent particular file.")


(defun teatime--has-buf-in-other-window ()
  "Checks if current buffer was opened in other window."
  (let ((win (next-window (selected-window)))
        (has-buffer nil))

    (while (and (not (eq win (selected-window)))
                (not has-buffer))

      (if (eq (current-buffer) (window-buffer win))
          (setq has-buffer t))

      (setq win (next-window (selected-window))))

    has-buffer))


(defun teatime-close-window ()
  "Close current buffer or window."
  (interactive)

  (when (not (teatime--has-buf-in-other-window))
    (if (and (buffer-modified-p)
             (not (member (buffer-name) teatime--service-buffer-names))
             (y-or-n-p (format "Do you want to save changes you made to %s?" (buffer-name))))
	    (save-buffer))

    (kill-buffer (current-buffer)))

  (when (not (one-window-p t))
    (delete-window)))

(defun teatime-new-untitled-file ()
  "Create new untitled buffer."
  (interactive)
  (let ((buf (generate-new-buffer "Untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    buf))

(defun teatime-save-file ()
  "Save current buffer."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(provide 'teatime-utils)
