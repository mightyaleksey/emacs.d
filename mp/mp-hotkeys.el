;;; -*- lexical-binding: t -*-

(require 'mp-editor)
(require 'mp-buffers)
(require 'mp-windows)

(if (display-graphic-p)
    (progn
      ;; https://www.reddit.com/r/emacs/comments/6adli0/preferred_syntax_for_defining_keys/dheggib/
      ;; (use-global-map (make-sparse-keymap))

      ;; unbind keys from ns-win.el
      (define-key global-map [?\s-,] nil)
      (define-key global-map [?\s-'] nil)
      (define-key global-map [?\s-`] nil)
      (define-key global-map [?\s-~] nil)
      (define-key global-map [?\s--] nil)
      (define-key global-map [?\s-:] nil)
      (define-key global-map [?\s-?] nil)
      (define-key global-map [?\s-^] nil)
      (define-key global-map [?\s-&] nil)
      (define-key global-map [?\s-C] nil)
      (define-key global-map [?\s-D] nil)
      (define-key global-map [?\s-E] nil)
      ;; (define-key global-map [?\s-L] 'shell-command)
      (define-key global-map [?\s-M] nil)
      (define-key global-map [?\s-S] nil)
      ;; (define-key global-map [?\s-a] 'mark-whole-buffer)
      ;; (define-key global-map [?\s-c] 'ns-copy-including-secondary)
      (define-key global-map [?\s-d] nil)
      (define-key global-map [?\s-e] nil)
      (define-key global-map [?\s-f] nil)
      (define-key global-map [?\s-g] nil)
      ;; (define-key global-map [?\s-h] 'ns-do-hide-emacs)
      ;; (define-key global-map [?\s-H] 'ns-do-hide-others)
      ;; (define-key global-map [?\M-\s-h] 'ns-do-hide-others)
      (define-key global-map [?\s-j] nil)
      (define-key global-map [?\s-k] nil)
      (define-key global-map [?\s-l] nil)
      (define-key global-map [?\s-m] nil)
      (define-key global-map [?\s-n] nil)
      (define-key global-map [?\s-o] nil)
      (define-key global-map [?\s-p] nil)
      ;; (define-key global-map [?\s-q] 'save-buffers-kill-emacs)
      ;; (define-key global-map [?\s-s] 'save-buffer)
      (define-key global-map [?\s-t] nil)
      (define-key global-map [?\s-u] nil)
      ;; (define-key global-map [?\s-v] 'yank)
      (define-key global-map [?\s-w] nil)
      ;; (define-key global-map [?\s-x] 'kill-region)
      (define-key global-map [?\s-y] nil)
      (define-key global-map [?\s-z] nil)
      (define-key global-map [?\s-+] nil)
      (define-key global-map [?\s-=] nil)
      (define-key global-map [?\s--] nil)
      (define-key global-map [?\s-0] nil)

      ;; unbind unused keys
      (setq mp-keys-to-unbind
            '(
              "C-a"
              "C-b"
              "C-e"
              "C-f"
              "C-k"
              "M-a"
              "M-b"
              "M-c"
              "M-d"
              "M-e"
              "M-f"
              "M-g"
              "M-h"
              "M-i"
              "M-j"
              "M-k"
              "M-l"
              "M-m"
              "M-n"
              "M-o"
              "M-p"
              "M-q"
              "M-r"
              "M-s"
              "M-t"
              "M-u"
              "M-v"
              "M-w"
              "M-x"
              "M-y"
              "M-z"
              ))

      (dolist (key mp-keys-to-unbind)
        (global-unset-key (kbd key)))
      ;; using alt instead of super
      ;; editing
      (global-set-key (kbd "M-<return>") 'mp-insert-line-after)
      (global-set-key (kbd "M-S-<return>") 'mp-insert-line-before)
      (global-set-key (kbd "M-<up>") 'mp-move-line-or-selection-up)
      (global-set-key (kbd "M-<down>") 'mp-move-line-or-selection-down)
      (global-set-key (kbd "M-l") 'mp-select-line)
      ;; select word - repeat to select next occurrence \s?-d
      ;; select all occurrences of current selection C-\s?-g
      ;; extra cursor on the line above C-S-up
      ;; extra cursor on the line below C-S-down
      (global-set-key (kbd "M-m") 'mp-jump-to-matching-parentheses)
      ;; select all contents of the current parentheses C-S-m
      (global-set-key (kbd "C-a") 'move-beginning-of-line)
      ;; move to beginning of text on line \s?-left
      (global-set-key (kbd "C-e") 'move-end-of-line)
      (global-set-key (kbd "C-k C-k") 'kill-line)
      ;; delete from cursor to start of line super k delete
      ;; indent current line super+]
      ;; un-indent current line super+[
      (global-set-key (kbd "M-D") 'mp-duplicate-line)
      (global-set-key (kbd "M-j") 'mp-join-line-below)
      (global-set-key (kbd "C-/") 'mp-comment-line)
      ;; block comment current selection super+meta+/
      ;; redo, or repeat last keyboard shortcur command super+y
      ;; paste and indent correctly super+shift+v
      ;; select next auto-complete suggestion ctrl+space
      ;; soft undo; jumps to your last change before undoing change when repeated ctrl+u
      ;; column selection up ctrl+shift+up
      ;; column selection down ctrl+shift+down
      (global-set-key (kbd "M-K") 'kill-whole-line)
      ;; navigation
      ;; quick-open files by name super+p or super+t
      ;; goto symbol super+r
      ;; goto line in current file ctrl+g
      ;; general
      (global-set-key (kbd "M-P") 'execute-extended-command)
      ;; find / replace
      (global-set-key (kbd "C-f") 'isearch-forward)
      ;; replace super+meta+f/
      ;; find in files super+shift+f
      ;; scrolling
      ;; scroll down one page ctrl+v
      (global-set-key (kbd "C-l") 'recenter-top-bottom)
      ;; scroll to end of file super+down
      ;; scroll to start of file super+up
      ;; buffers
      (global-set-key (kbd "M-T") 'mp-open-last-closed-buffer)
      ;; jump to tab in current group super+num
      (global-set-key (kbd "M-{") 'mp-switch-to-previous-buffer)
      (global-set-key (kbd "M-}") 'mp-switch-to-next-buffer)
      ;; split window
      (global-set-key (kbd "C-M-1") 'mp-close-other-windows)
      (global-set-key (kbd "C-M-2") 'mp-split-view-into-columns-2)
      (global-set-key (kbd "C-M-3") 'mp-split-view-into-columns-3)
      (global-set-key (kbd "C-M-4") 'mp-split-view-into-columns-4)
      (global-set-key (kbd "C-1") 'mp-jump-to-window-1)
      (global-set-key (kbd "C-2") 'mp-jump-to-window-2)
      (global-set-key (kbd "C-3") 'mp-jump-to-window-3)
      (global-set-key (kbd "C-4") 'mp-jump-to-window-4)
      (global-set-key (kbd "C-5") 'mp-jump-to-window-5)
      ;; text manipulation
      (global-set-key (kbd "M-k M-u") 'upcase-region)
      (global-set-key (kbd "M-k M-l") 'downcase-region)
      ;; extra
      (global-set-key (kbd "M-z") 'toggle-truncate-lines)
      (global-set-key (kbd "C-w") 'kill-current-buffer)
      (global-set-key (kbd "C-o") 'find-file)
      (global-set-key (kbd "C-a") 'mark-whole-buffer)
      (global-set-key (kbd "C-s") 'save-buffer)
      (global-set-key (kbd "C-z") 'undo)
      (global-set-key (kbd "C-n") 'mp-switch-to-empty-buffer)
      )
  (progn
    ;; editing
    (define-key global-map (kbd "M-RET") 'mp-insert-line-after)
    ;; (define-key global-map (kbd "M-S-RET") 'mp-insert-line-before)
    ;; (define-key global-map (kbd "M-<up>") 'mp-move-line-or-selection-up)
    ;; (define-key global-map (kbd "M-<down>") 'mp-move-line-or-selection-down)
    (define-key global-map (kbd "C-l") 'mp-select-line)

    (define-key global-map (kbd "M-m") 'mp-jump-to-matching-parentheses) ;; C-m == return
    (define-key global-map (kbd "C-k") 'kill-line)
    ;; (define-key global-map (kbd "C-D") 'mp-duplicate-line)
    (define-key global-map (kbd "C-j") 'mp-join-line-below)
    (define-key global-map (kbd "C-/") 'mp-comment-line)
    ;; general
    (define-key global-map (kbd "M-P") 'execute-extended-command)
    )
  )


(provide 'mp-hotkeys)
