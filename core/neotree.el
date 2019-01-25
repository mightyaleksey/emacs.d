;;; Package --- Summary
;;; neotree

(use-package neotree
  :defer t

  :bind
  (("C-b"      . dt-neotree-show)
    :map neotree-mode-map
    ("C-b"     . dt-neotree-hide)
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
      (neotree-dir (dt-project-dir))
      (neotree-find file)))

  (defun dt-neotree-hide ()
    "Close the NeoTree window."
    (interactive)
    (neotree-hide)
    (xah-fly-command-mode-activate))

  )
