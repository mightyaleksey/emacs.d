;;; Package --- Summary
;;; editorconfig - sets the coding styles through the .editorconfig file

;;; editorconfig-after-apply-functions - hook for setting custom indent values

(use-package editorconfig
  :defer nil

  :config
  (add-hook 'editorconfig-after-apply-functions
    (lambda (cfg)
      (let ((indent (string-to-number (gethash 'indent_size cfg "2"))))
        (setq js-indent-level indent))))

  (editorconfig-mode 1))
