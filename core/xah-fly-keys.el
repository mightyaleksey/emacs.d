;;; Package --- Summary
;;; xah-fly-keys - A modal keybinding for emacs (like vim)

;;; @see https://github.com/xahlee/xah-fly-keys

(defun split-window-below-and-focus ()
  "Split window on two and focus one below."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun split-window-right-and-focus ()
  "Split window on two and focus one on right."
  (interactive)
  (split-window-right)
  (other-window 1))


(use-package xah-fly-keys
  :init
  (setq xah-fly-use-control-key nil)

  :config
  (xah-fly-keys-set-layout "qwerty")

  (defun xah-fly-command-mode-init ()
    "Set command mode keys."
    (interactive)
    (dt-map-keys
      xah-fly-key-map
      '(
         ("~" . nil)
         (":" . nil)

         ("SPC" . xah-fly-leader-key-map)
         ("DEL" . xah-fly-leader-key-map)

         ("1" . xah-extend-selection)
         ("2" . xah-select-line)
         ("3" . delete-other-windows)
         ("4" . split-window-below-and-focus)
         ("5" . delete-char)
         ("6" . xah-select-block)
         ("7" . xah-select-line)
         ("8" . xah-extend-selection)
         ("9" . xah-select-text-in-quote)
         ("0" . xah-pop-local-mark-ring)
         ("-" . xah-backward-punct)
         ("=" . xah-forward-punct)

         ("q" . xah-reformat-lines)
         ("w" . xah-shrink-whitespaces)
         ("e" . xah-backward-kill-word)
         ("r" . xah-kill-word)
         ("t" . set-mark-command)
         ("y" . undo)
         ("u" . backward-word)
         ("i" . previous-line)
         ("o" . forward-word)
         ("p" . xah-insert-space-before)
         ("[" . hippie-expand)
         ;; ("]" . xah-forward-equal-sign)
         ("a" . execute-extended-command)
         ("s" . open-line)
         ("d" . xah-delete-backward-char-or-bracket-text)
         ("f" . xah-fly-insert-mode-activate)
         ("g" . xah-delete-current-text-block)
         ("h" . xah-beginning-of-line-or-block)
         ("j" . backward-char)
         ("k" . next-line)
         ("l" . forward-char)
         (";" . xah-end-of-line-or-block)
         ;; ("'" . xah-cycle-hyphen-underscore-space)
         ("'" . nil)
         ("\\" . nil)
         ("`" . other-frame)
         ("z" . xah-comment-dwim)
         ("x" . xah-cut-line-or-region)
         ("c" . xah-copy-line-or-region)
         ("v" . xah-paste-or-paste-previous)
         ("b" . xah-toggle-letter-case)
         ("n" . isearch-forward)
         ("m" . xah-backward-left-bracket)
         ("," . xah-next-window-or-frame)
         ("." . xah-forward-right-bracket)
         ("/" . xah-goto-matching-bracket)
         ))

    (define-key xah-fly-key-map (kbd "a")
      (if (fboundp 'smex) 'smex (if (fboundp 'helm-M-x) 'helm-M-x 'execute-extended-command)))

    (progn
      (setq xah-fly-insert-state-q nil)
      (modify-all-frames-parameters (list (cons 'cursor-type 'box))))

    (setq mode-line-front-space "C")
    (force-mode-line-update))


  (defun xah-fly-insert-mode-init ()
    "Set insertion mode keys."
    (interactive)
    (dt-map-keys
      xah-fly-key-map
      '(
         ("~" . nil)
         (":" . nil)

         ("SPC" . nil)
         ("DEL" . nil)

         ("1" . nil)
         ("2" . nil)
         ("3" . nil)
         ("4" . nil)
         ("5" . nil)
         ("6" . nil)
         ("7" . nil)
         ("8" . nil)
         ("9" . nil)
         ("0" . nil)
         ("-" . nil)
         ("=" . nil)

         ("q" . nil)
         ("w" . nil)
         ("e" . nil)
         ("r" . nil)
         ("t" . nil)
         ("y" . nil)
         ("u" . nil)
         ("i" . nil)
         ("o" . nil)
         ("p" . nil)
         ("[" . nil)
         ("]" . nil)
         ("a" . nil)
         ("s" . nil)
         ("d" . nil)
         ("f" . nil)
         ("g" . nil)
         ("h" . nil)
         ("j" . nil)
         ("k" . nil)
         ("l" . nil)
         (";" . nil)
         ("'" . nil)
         ("\\" . nil)
         ("`" . nil)
         ("z" . nil)
         ("x" . nil)
         ("c" . nil)
         ("v" . nil)
         ("b" . nil)
         ("n" . nil)
         ("m" . nil)
         ("," . nil)
         ("." . nil)
         ("/" . nil)
         ))

    (progn
      (setq xah-fly-insert-state-q t)
      (modify-all-frames-parameters (list (cons 'cursor-type 'bar))))

    (setq mode-line-front-space "I")
    (force-mode-line-update))


  (dt-map-keys
    xah-fly-leader-key-map
    '(
       ("4" . split-window-right-and-focus)
       ))


  (dt-map-keys
    xah-fly-key-map
    '(
       ("C-a"  . mark-whole-buffer)
       ("C-n"  . xah-new-empty-buffer)
       ("C-o"  . find-file)
       ("C-s"  . save-buffer)
       ("C-v"  . yank)
       ("C-w"  . xah-close-current-buffer)
       ("C-z"  . undo)
       ("C-\\" . comment-line)
       ))


  (defun xah-fly-keys-russian-on ()
    "Add russian layout keys."
    (interactive)
    (progn
      (dt-map-keys
        xah-fly-key-map
        '(
           ("й" . xah-reformat-lines)
           ("ц" . xah-shrink-whitespaces)
           ("у" . xah-backward-kill-word)
           ("к" . xah-kill-word)
           ("е" . set-mark-command)
           ("н" . undo)
           ("г" . backward-word)
           ("ш" . previous-line)
           ("щ" . forward-word)
           ("з" . xah-insert-space-before)
           ("х" . hippie-expand)
           ("ф" . execute-extended-command)
           ("ы" . open-line)
           ("в" . xah-delete-backward-char-or-bracket-text)
           ("а" . xah-fly-insert-mode-activate)
           ("п" . xah-delete-current-text-block)
           ("р" . xah-beginning-of-line-or-block)
           ("о" . backward-char)
           ("л" . next-line)
           ("д" . forward-char)
           ("ж" . xah-end-of-line-or-block)
           ("э" . xah-cycle-hyphen-underscore-space)
           ("ё" . nil)
           ("я" . xah-comment-dwim)
           ("ч" . xah-cut-line-or-region)
           ("с" . xah-copy-line-or-region)
           ("м" . xah-paste-or-paste-previous)
           ("и" . xah-toggle-letter-case)
           ("т" . isearch-forward)
           ("ь" . xah-backward-left-bracket)
           ("б" . xah-next-window-or-frame)
           ("ю" . xah-forward-right-bracket)
           ))))


  (defun xah-fly-keys-russian-off ()
    "Remove russian layout keys."
    (interactive)
    (progn
      (dt-map-keys
        xah-fly-key-map
        '(
           ("й" . nil)
           ("ц" . nil)
           ("у" . nil)
           ("к" . nil)
           ("е" . nil)
           ("н" . nil)
           ("г" . nil)
           ("ш" . nil)
           ("щ" . nil)
           ("з" . nil)
           ("х" . nil)
           ("ф" . nil)
           ("ы" . nil)
           ("в" . nil)
           ("а" . nil)
           ("п" . nil)
           ("р" . nil)
           ("о" . nil)
           ("л" . nil)
           ("д" . nil)
           ("ж" . nil)
           ("э" . nil)
           ("ё" . nil)
           ("я" . nil)
           ("ч" . nil)
           ("с" . nil)
           ("м" . nil)
           ("и" . nil)
           ("т" . nil)
           ("ь" . nil)
           ("б" . nil)
           ("ю" . nil)
           ))))


  (add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-keys-russian-on)
  (add-hook 'xah-fly-insert-mode-activate-hook 'xah-fly-keys-russian-off)


  (global-set-key (kbd "<home>") 'xah-fly-command-mode-activate-no-hook)

  (xah-fly-keys 1))


(use-package which-key
  :config (which-key-mode))
