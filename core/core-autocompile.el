;; emacs -batch -f batch-byte-compile **/*.el

(defvar mp-home-directory (expand-file-name user-emacs-directory)
  "Place where core files located.")

(defun mp-autocompile-core-files ()
  "Byte-compile all core files."
  (interactive)
  (require 'bytecomp)
  (byte-recompile-directory (concat mp-home-directory "core/") 0)
  (byte-recompile-file (concat mp-home-directory "init.el") nil 0)
  )

(defun mp-autocompile-on-init ()
  "Byte-compile all core files on first load."
  (let ((mp-init-file (expand-file-name "init.el" user-emacs-directory)))

    (unless (file-exists-p (concat mp-init-file "c"))
      (mp-autocompile-core-files)))
  )

(defun mp-autocompile-on-save ()
  "Recompile core file on save."
  (when (and
         (string-prefix-p mp-home-directory (buffer-file-name))
         (file-exists-p (concat (buffer-file-name) "c")))

    (require 'bytecomp)
    (byte-recompile-file buffer-file-name))
  )

(add-hook 'after-init-hook 'mp-autocompile-on-init)
(add-hook 'after-save-hook 'mp-autocompile-on-save)

(provide 'core-autocompile)
