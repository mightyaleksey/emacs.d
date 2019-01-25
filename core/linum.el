;;; Package --- Summary
;;; linum - shows lines number

;;; @todo skip for commit messages

(use-package linum
  :init
  (setq linum-format " %d ")

  :config
  (global-linum-mode 1)

  (defcustom linum-disabled-modes-list
    '(
       compilation-mode
       dired-mode
       doc-view-mode
       eshell-mode
       fundamental-mode ;; git commit
       image-mode
       org-mode
       text-mode
       )
    "* List of modes disabled when global linum mode is on"
    :type '(repeat (sexp :tag "Major mode"))
    :tag " Major modes where linum is disabled: "
    :group 'linum)

  (defun linum-on ()
    (unless (or
              (minibufferp)
              (string-match "*" (buffer-name))
              (member major-mode linum-disabled-modes-list)
              (> (buffer-size) 50000))
      (linum-mode 1)))

  )
