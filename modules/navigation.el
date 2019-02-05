(defun dt-map-keys (@keymap-name @key-cmd-alist)
  "Define keys in a declarative way."
  (interactive)
  (mapc
    (lambda ($pair)
      (define-key @keymap-name (kbd (car $pair)) (cdr $pair)))
    @key-cmd-alist))

(defun insert-mdash ()
  "Insert medium dash symbol."
  (interactive)
  (insert-char 8212))

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
       ("C-7"  . xah-previous-user-buffer)
       ("C-8"  . xah-next-user-buffer)

       ("C-9"  . scroll-down-command)
       ("C-0"  . scroll-up-command)

       ("C-a"  . mark-whole-buffer)
       ("C-n"  . xah-new-empty-buffer)
       ("C-o"  . find-file)
       ("C-s"  . save-buffer)
       ("C-v"  . yank)
       ("C-w"  . xah-close-current-buffer)
       ("C-z"  . undo)
       ("C-\\" . comment-line)

       ("C-ф"  . mark-whole-buffer)
       ("C-т"  . xah-new-empty-buffer)
       ("C-щ"  . find-file)
       ("C-ы"  . save-buffer)
       ("C-м"  . yank)
       ("C-ц"  . xah-close-current-buffer)
       ("C-я"  . undo)

       ("M-_"  . insert-mdash)
       ))

  (when (not (display-graphic-p))
    (dt-map-keys
      xah-fly-key-map
      '(
         ("ESC <up>"   . backward-paragraph)
         ("ESC <down>" . forward-paragraph)
         ("M-b"        . xah-previous-user-buffer)
         ("M-f"        . xah-next-user-buffer)
         )))


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
  (global-set-key (kbd "M-_") 'insert-mdash)

  (xah-fly-keys 1))


(use-package which-key
  :config (which-key-mode))


(use-package ivy
  :defer t

  :bind
  (:map ivy-minibuffer-map
    ("C-M-m" . ivy-immediate-done) ; use current input instead of current candidate (used fo file creation)
    )

  :init
  (setq ivy-count-format "")
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist '((read-file-name-internal . ivy--regex-plus)
                                 (t . ivy--regex-fuzzy)))

  (ivy-mode 1))


(use-package neotree
  :defer t

  :bind
  (("C-b"      . dt-neotree-show)
    ("C-и"     . dt-neotree-show)
    :map neotree-mode-map
    ("C-b"     . dt-neotree-hide)
    ("C-и"     . dt-neotree-hide)
    (","       . xah-next-window-or-frame)
    ;;; movement keys
    ("i"       . neotree-previous-line)
    ("k"       . neotree-next-line)
    ("j"       . neotree-collapse-all)
    ("l"       . neotree-enter)
    ("<left>"  . neotree-collapse-all)
    ("<right>" . neotree-enter)
    ;;; modification
    ("n"       . neotree-create-node)
    ("m"       . neotree-rename-node)
    ("r"       . neotree-delete-node)
    ;;; other
    ("c"       . neotree-change-root)
    ("d"       . neotree-dir)
    ("q"       . dt-neotree-hide)
    ("e"       . neotree-enter)
    )

  :hook
  (neo-enter . (lambda (type &rest arg)
                 (if (and (not (eq (neo-global--get-window)
                                 (selected-window)))
                       (equal type 'file))
                   (progn
                     (neotree-hide)
                     (xah-fly-command-mode-activate))
                   )))
  (neo-after-create . (lambda (&rest arg) (xah-fly-insert-mode-activate)))

  :config
  (setq neo-show-hidden-files t)
  (setq neo-show-updir-line nil)
  (setq neo-smart-open t)
  (setq neo-theme 'arrow)
  (setq neo-window-position 'right)

  ;;; show only directory name on the top
  (defun neo-buffer--insert-root-entry (node)
    (neo-buffer--node-list-set nil node)
    (insert
      (propertize (neo-path--file-short-name node) 'face 'neo-root-dir-face))
    (neo-buffer--newline-and-begin))


  ;;; custom helpers
  (defun dt-neotree-show ()
    "Open the NeoTree window."
    (interactive)
    (let ((file buffer-file-name))
      (neotree-dir (vcs-project-dir))
      (neotree-find file)))

  (defun dt-neotree-hide ()
    "Close the NeoTree window."
    (interactive)
    (neotree-hide)
    (xah-fly-command-mode-activate))

  )


(use-package find-file-in-project
  :defer t

  :bind
  ("C-p" . dt-find-file-in-project)
  ("C-з" . dt-find-file-in-project)

  :config
  (setq ffip-find-options "-maxdepth 5") ; Saves from hanging in the "~/" directory

  (defun dt-find-file-in-project ()
    (interactive)
    (setq ffip-project-root (vcs-project-dir))
    (find-file-in-project))

  )


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
