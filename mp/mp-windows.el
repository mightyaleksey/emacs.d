;;; -*- lexical-binding: t -*-

;; Set of functions for window manipulation.
;; Inspired by https://www.emacswiki.org/emacs/window-number.el

(require 'cl)

(defun mp-window-count ()
  "Returns number of opened windows including minibuffer."
  (length (window-list (selected-frame) t)))

(defun mp-window-ordered-list ()
  "Returns list of windows in fixed order."
  (let ((minibuffer-window (car
          (set-difference
           (window-list (selected-frame) t)
           (window-list (selected-frame) 1)))))
    (let ((current-window minibuffer-window)
    (list nil))
      (while (progn
         (setq current-window (next-window current-window t))
         (setq list (cons current-window list))
         (not (eq current-window minibuffer-window))))
      (reverse list)))
  )

(defun mp-window-by-number (number)
  "Returns window by it's number."
  (nth (- number 1) (mp-window-ordered-list)))

(defun mp-jump-to-window (number)
  "Selects window by it's number."
  (if (integerp number)
      (let ((window (mp-window-by-number number)))
  (if (and window
     (or (not (window-minibuffer-p window))
         (minibuffer-window-active-p window)))
      (select-window window)
    (message (format "No window with number %s" number)))))
  )

(defun mp-move-buffer-to-window (number)
  "Move active buffer to another window by it's number."
  (let ((target-window (mp-window-by-number number))
  (current-window (selected-window)))
    (if (not (eq target-window current-window))
  (let ((current-window-buffer (window-buffer current-window)))
    (set-window-buffer current-window (window-buffer target-window))
    (set-window-buffer target-window current-window-buffer))))
  )

(defun mp-close-window-or-buffer ()
  (interactive)
  (let ((window-count (mp-window-count)))
    (if (> window-count 2)
        (progn
          (delete-window)
          (balance-windows))
      (mp-close-current-buffer))))

(defun mp-close-other-windows ()
  (interactive)
  (delete-other-windows))

(defun mp-split-view-into-columns-2 ()
  (interactive)
  (split-window-horizontally)
  (balance-windows))

(defun mp-split-view-into-columns-3 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun mp-split-view-into-columns-4 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

;; aliases

(defun mp-jump-to-window-1 ()
  "Selects the first window."
  (interactive)
  (mp-jump-to-window 1))

(defun mp-jump-to-window-2 ()
  "Selects the second window."
  (interactive)
  (mp-jump-to-window 2))

(defun mp-jump-to-window-3 ()
  "Selects the third window."
  (interactive)
  (mp-jump-to-window 3))

(defun mp-jump-to-window-4 ()
  "Selects the fourth window."
  (interactive)
  (mp-jump-to-window 4))

(defun mp-jump-to-window-5 ()
  "Selects the fourth window."
  (interactive)
  (mp-jump-to-window 5))

(defun mp-move-buffer-to-window-1 ()
  (interactive)
  (mp-move-buffer-to-window 1))

(defun mp-move-buffer-to-window-2 ()
  (interactive)
  (mp-move-buffer-to-window 2))

(defun mp-move-buffer-to-window-3 ()
  (interactive)
  (mp-move-buffer-to-window 3))

(defun mp-move-buffer-to-window-4 ()
  (interactive)
  (mp-move-buffer-to-window 4))

(provide 'mp-windows)
