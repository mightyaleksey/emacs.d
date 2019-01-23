;;; Package --- Summary
;;; find-file-in-project

(use-package find-file-in-project
  :defer t

  :bind
  ("C-p" . dt-find-file-in-project)

  :config
  (setq ffip-find-options "-maxdepth 4") ; Saves from hanging in the "~/" directory

  (defun dt-find-file-in-project ()
    (interactive)
    (let ((dir))
      (if buffer-file-name
        (setq dir buffer-file-name)
        (setq dir default-directory))
      (setq ffip-project-root (dt-project-dir dir))
      (find-file-in-project))))
