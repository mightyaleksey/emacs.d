;;; Package --- Summary
;;; editorconfig - sets the coding styles through the .editorconfig file

;;; @todo handle other props related to whitespaces and etc.

(use-package editorconfig
  :defer nil

  :config
  (add-hook 'editorconfig-after-apply-functions
    (lambda (cfg)
      (let ((indent (string-to-number (gethash 'indent_size cfg "2"))))
        (setq js-indent-level indent))))

  (editorconfig-mode 1))
