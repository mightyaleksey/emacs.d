(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)

  (setq-default line-spacing 2)
  (set-face-attribute 'default nil
    :family "Meslo LG S"
    :height 140
    :weight 'normal
    :width 'normal))


(setq inhibit-startup-screen t)
(column-number-mode)


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


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
      (setq frame-title-format '(buffer-file-name "%f" ("%b")))))

  )


(use-package fireplace
  :defer t)
