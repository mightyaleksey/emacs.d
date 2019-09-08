(require 'core-workspace)
(require 'neotree)

(setq neo-show-hidden-files t)
(setq neo-show-updir-line nil)
(setq neo-smart-open t)
(setq neo-theme 'arrow)
(setq neo-window-position 'right)

(define-key neotree-mode-map "q" 'workspace-hide-project)
(define-key neotree-mode-map "[" 'neotree-previous-line)

(define-key neotree-mode-map ";" 'neotree-collapse-all)
(define-key neotree-mode-map "'" 'neotree-next-line)
(define-key neotree-mode-map "\\" 'neotree-enter)

(defun mp-neotree-going-to-show (&rest args)
  (keys-insertion-mode-activate)
)

(add-hook 'neo-after-create-hook 'mp-neotree-going-to-show)

(defun mp-neotree-going-to-hide (type &rest args)
  (if (and (not (eq (neo-global--get-window)
                    (selected-window)))
           (equal type 'file))

      (progn
        (neotree-hide) 
        (keys-command-mode-activate))
    )
)

(add-hook 'neo-enter-hook 'mp-neotree-going-to-hide)

(defun neo-buffer--insert-root-entry (node)
  "Show only directory name on the top."
  (neo-buffer--node-list-set nil node)
  (insert
   (propertize (neo-path--file-short-name node) 'face 'neo-root-dir-face))
  (neo-buffer--newline-and-begin))

(defun workspace-show-project ()
  "Open the neotree window."
  (interactive)
  (neotree-show)
  (neotree-dir (workspace-get-directory))
  (neotree-find (buffer-file-name))
  )

(defun workspace-hide-project ()
  "Close the neotree window."
  (interactive)
  (neotree-hide)
  (keys-command-mode-activate)
  )

(provide 'core-neotree)
