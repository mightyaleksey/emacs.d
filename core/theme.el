;;; Package --- Summary
;;; color theme

;;; borrowed from Nicolas Petton,
;;; @see https://github.com/NicolasPetton/emacs.d/blob/master/init.el

(use-package zerodark-theme
  :demand t

  :config
  (progn
    (defun set-selected-frame-dark ()
      (interactive)
      (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
        (call-process-shell-command
         (format
          "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
          frame-name))))

    (when (window-system)
      (load-theme 'zerodark t)
      ;; (zerodark-setup-modeline-format)
      (set-selected-frame-dark)
      (setq frame-title-format '(buffer-file-name "%f" ("%b"))))))
