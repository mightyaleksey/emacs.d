;;; http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/

(defun mp-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))

    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output)
  )

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name) (concat " " (mp-shorten-directory default-directory 20)) " "))
    face mode-line)
  "Formats the current directory.")

(put 'mode-line-directory 'risky-local-variable t)

(setq mode-line-position
      '(;; %p print percent of buffer above top of window, o Top, Bot or All
        ;; (-3 "%p")
        ;; %I print the size of the buffer, with kmG etc
        ;; (size-indication-mode ("/" (-4 "%I")))
        ;; " "
        ;; %l print the current line number
        ;; %c print the current column
        (line-number-mode ("%l" (column-number-mode ":%c")))))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                ;; mode-line-mule-info -- I'm always on utf-8
                mode-line-client
                mode-line-modified
                ;; mode-line-remote -- no need to indicate this specially
                ;; mode-line-frame-identification -- this is for text-mode emacs only
                " "
                mode-line-directory
                mode-line-buffer-identification
                " "
                mode-line-position
                ;;(vc-mode vc-mode)  -- I use magit, not vc-mode
                (flycheck-mode flycheck-mode-line)
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces))

(provide 'core-ui)
