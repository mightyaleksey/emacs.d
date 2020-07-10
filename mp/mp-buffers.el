;;; -*- lexical-binding: t -*-

(defvar mp-recently-closed-buffers nil)
(defvar mp-recently-closed-buffers-max 10)

(defun mp-user-buffer-p ()
  (interactive)
  (cond
   ((string-equal (substring (buffer-name) 0 1) "*") nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   (t t)))

(defun mp-close-current-buffer ()
  (interactive)

  (if (string= major-mode "minibuffer-inactive-mode")
      (minibuffer-keyboard-quit)
    (progn
      (when (and
             (buffer-modified-p)
             (mp-user-buffer-p)
             (if (equal (buffer-file-name) nil)
                 (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
               t))
        (if (y-or-n-p (format "Do you want to save the changes you made to %s? " (buffer-name)))
            (save-buffer)
          (set-buffer-modified-p nil)))

      (when (buffer-file-name)
        (setq mp-recently-closed-buffers
              (cons (cons (buffer-name) (buffer-file-name)) mp-recently-closed-buffers))
        (when (> (length mp-recently-closed-buffers) mp-recently-closed-buffers-max)
          (setq mp-recently-closed-buffers (butlast mp-recently-closed-buffers 1))))

      (kill-buffer (current-buffer)))))

(defun mp-open-last-closed-buffer ()
  (interactive)
  (if (> (length mp-recently-closed-buffers) 0)
      (find-file (cdr (pop mp-recently-closed-buffers)))
    (progn (message "No recently closed buffer in this session."))))

(defun mp-switch-to-empty-buffer ()
  (interactive)
  (let ((mp-buf (generate-new-buffer "untitled")))
    (switch-to-buffer mp-buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    mp-buf))

(defun mp-switch-to-previous-buffer ()
  (interactive)
  (previous-buffer))

(defun mp-switch-to-next-buffer ()
  (interactive)
  (next-buffer))


(provide 'mp-buffers)
