;;; Package --- Summary
;;; linum - shows lines number

;;; @todo skip for commit messages

(use-package linum
  :init (setq linum-format " %d ")
  :config (global-linum-mode 1))
