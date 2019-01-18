;;; Package --- Summary
;;; neotree

(use-package neotree
  :defer t

  :bind
  (("C-b" . dt-neotree-toggle)
    :map neotree-mode-map
    ("RET" . dt-neotree-enter)
    (","   . dt-neotree-next-window-or-frame)
    ;;; movement keys
    ("i"   . neotree-previous-line)
    ("k"   . neotree-next-line)
    ("j"   . neotree-collapse-all)
    ("l"   . dt-neotree-enter)
    ;;; modification
    ("n"   . neotree-create-node)
    ("m"   . neotree-rename-node)
    ("r"   . neotree-delete-node)
    ;;; other
    ("c"   . neotree-change-root)
    ("d"   . neotree-dir)
    ("q"   . dt-neotree-hide)
    ("e"   . dt-neotree-enter))

  :hook
  (neo-after-create . dt-neotree-activate)

  :config
  (setq neo-show-hidden-files t)
  (setq neo-show-updir-line nil)
  (setq neo-smart-open t)
  (setq neo-window-position 'right)

  (defun dt-neotree-activate (&rest arg)
    (xah-fly-insert-mode-activate))

  (defun dt-neotree-enter ()
    "NeoTree open file / unfold directory."
    (interactive)
    (if (file-directory-p (neo-buffer--get-filename-current-line))
      (neotree-enter)
      (progn
        (neotree-enter)
        (dt-neotree-hide))))

  (defun dt-neotree-hide ()
    "Close the NeoTree window."
    (interactive)
    (neotree-hide)
    (xah-fly-command-mode-activate))

  (defun dt-neotree-toggle ()
    "Toggle show the NeoTree window."
    (interactive)
    (if (neo-global--window-exists-p)
      (progn
        (neotree-hide)
        (xah-fly-command-mode-activate))
      (neotree-show)))

  (defun dt-neotree-next-window-or-frame ()
    "Switch to next window or frame."
    (interactive)
    (xah-next-window-or-frame)
    (xah-fly-command-mode-activate))


  ;;; show only directory name on the top
  (defun neo-buffer--insert-root-entry (node)
    (neo-buffer--node-list-set nil node)
    (insert
      (propertize (neo-path--file-short-name node) 'face 'neo-root-dir-face))
    (neo-buffer--newline-and-begin))

  )
