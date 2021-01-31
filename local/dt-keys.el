;;; -*- lexical-binding: t -*-
(require 'dt-utils)

;; '(lambda () (interactive)(dt-todo "insert line above"))
(global-set-key (kbd "<backtab>") 'dt-back-indent-region)
(global-set-key (kbd "RET") 'dt-newline-and-indent)
(global-set-key (kbd "<s-return>") 'dt-insert-line-below)
(global-set-key (kbd "<S-s-return>") 'dt-insert-line-above)
(global-set-key (kbd "<M-down>") 'dt-move-line-down)
(global-set-key (kbd "<M-up>") 'dt-move-line-up)

(global-set-key (kbd "s-w") 'dt-close-buffer)
(global-set-key (kbd "s-o") 'dt-open-file)
(global-set-key (kbd "s-p") '(lambda () (interactive)(dt-todo "go to file")))
(global-set-key (kbd "s-{") 'dt-open-previous-buffer)
(global-set-key (kbd "s-}") 'dt-open-next-buffer)

(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-f") 'isearch-forward)
(global-set-key (kbd "s-j") 'dt-join-lines)
(global-set-key (kbd "s-l") 'dt-expand-line-selection)

(global-set-key (kbd "M-z") 'toggle-truncate-lines)
(global-set-key (kbd "s-n") 'dt-new-untitled-buffer)
(global-set-key (kbd "s-/") 'dt-toggle-line-comment)

(global-set-key (kbd "s-T") 'dt-reopen-closed-buffer)
(global-set-key (kbd "s-P") 'execute-extended-command)
(global-set-key (kbd "s-D") 'dt-copy-line-down)
(global-set-key (kbd "C-K") 'dt-delete-line)
(global-set-key (kbd "s-K") 'dt-delete-line)

;; customize find hotkeys
(define-key isearch-mode-map (kbd "<S-return>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "ESC") 'isearch-exit)
(define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)

(provide 'dt-keys)
