;;; Package --- Summary
;;; multiple-cursors

;;; the problem with return key in the terminal
;;; @see http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html

(use-package multiple-cursors
  :commands
  (
    mc/mark-next-like-this
	  mc/mark-next-like-this-word
	  mc/skip-to-next-like-this
	  mc/edit-ends-of-lines)

  :bind
  (
    ("C-k" . mc/mark-next-like-this)
	  ("C-d" . mc/mark-next-like-this-word)
	  ("C-f" . mc/skip-to-next-like-this)
	  ("C-l" . mc/edit-ends-of-lines))

  :config
  (setq mc/cmds-to-run-for-all
    '(
       xah-beginning-of-line-or-block
       xah-end-of-line-or-block
       xah-copy-line-or-region
       xah-paste-or-paste-previous
       xah-backward-kill-word
       xah-kill-word
       xah-insert-space-before
       )))
