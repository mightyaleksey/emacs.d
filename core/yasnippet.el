;;; Package --- Summary
;;; yasnippet

(use-package yasnippet
  :defer nil

  :config
  (defun dt-file-name (&optional default)
    "Gets the file name of the current buffer. In case it's \"index\" - returns the folder name instead.
Can be used for setting default component name or something similar."
    (interactive)
    (if buffer-file-name
      (let* ((file buffer-file-name)
              (name (file-name-base file)))

        (if (equal name "index")
          (setq name (file-name-base
                       (directory-file-name
                         (file-name-directory file)))))

          name)
        default))

  (setq yas-snippets-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))
