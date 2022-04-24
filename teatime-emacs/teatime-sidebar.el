;; -*- lexical-binding: t -*-

(defvar teatime-sidebar--buffer-name "*Sidebar*"
  "")
(defvar teatime-sidebar--current-window nil
  "Window that shows sidebar.")


(defun teatime-sidebar-quit ()
  (interactive)

  (when teatime-sidebar--current-window
    (select-window teatime-sidebar--current-window)
    (switch-to-buffer
     (get-buffer-create teatime-sidebar--buffer-name))

    ;; clean up
    (setq teatime-sidebar--current-window nil)

    (kill-buffer-and-window)))


(defun teatime-sidebar (&optional directory)
  (interactive)

  (unless directory
    (if default-directory
        (setq directory default-directory)
      (user-error "No directory")))

  (if (not teatime-sidebar--current-window)
      (progn
        (setq teatime-sidebar--current-window
              (split-window (frame-root-window) -30 'left))

        (select-window teatime-sidebar--current-window)
        (switch-to-buffer
         (get-buffer-create teatime-sidebar--buffer-name))

        (with-current-buffer teatime-sidebar--buffer-name
          (setq default-directory (file-name-as-directory directory)))

        (linum-mode 0)
        (visual-line-mode 0)

        ;; (setq-local mode-line-format nil)
      )

    (teatime-sidebar-quit)))

(provide 'teatime-sidebar)
