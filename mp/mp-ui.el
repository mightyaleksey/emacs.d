;;; -*- lexical-binding: t -*-

;; (set-face-background 'fringe "white")

(setq initial-scratch-message ";; hack the world\n\n")
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default truncate-lines t)
(setq-default global-visual-line-mode t)


(if window-system
    (load-theme 'doom-spacegrey t)
  )


(provide 'mp-ui)
