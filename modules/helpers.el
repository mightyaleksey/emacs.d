(setq css-indent-offset 2)
(setq js-indent-level 2)

(use-package editorconfig
  :defer nil

  :config
  (add-hook 'editorconfig-after-apply-functions
    (lambda (cfg)
      (let ((indent (string-to-number (gethash 'indent_size cfg "2"))))
        (setq js-indent-level indent))))

  (editorconfig-mode 1))


(use-package linum
  :init
  (setq linum-format " %d ")

  :config
  (global-linum-mode 1)

  (defcustom linum-disabled-modes-list
    '(
       compilation-mode
       dired-mode
       doc-view-mode
       eshell-mode
       fundamental-mode ;; git commit
       image-mode
       org-mode
       text-mode
       )
    "* List of modes disabled when global linum mode is on"
    :type '(repeat (sexp :tag "Major mode"))
    :tag " Major modes where linum is disabled: "
    :group 'linum)

  (defun linum-on ()
    (unless (or
              (minibufferp)
              (string-match "*" (buffer-name))
              (member major-mode linum-disabled-modes-list)
              (> (buffer-size) 50000))
      (linum-mode 1)))

  )


(use-package company
  :defer nil

  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-backends '((company-capf
                             company-yasnippet
                             company-files
                             company-keywords)
                            (company-dabbrev-code)
                            company-abbrev))
  (global-company-mode 1))


(use-package flycheck
  :defer t)


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


(use-package smartparens
  :init
  (smartparens-global-mode t))


(use-package popup
  :defer t

  :bind
  ("C-c C-h" . describe-thing-in-popup)

  :config
  (defun describe-thing-in-popup ()
    "Show description of a function at point in a popup."
    (interactive)
    (let* ((thing (symbol-at-point))
            (description (save-window-excursion
                           (describe-function thing)
                           (switch-to-buffer "*Help*")
                           (buffer-string))))
      (popup-tip description
        :point (point)
        :around t
        :height 30
        :scroll-bar t
        :margin t)))

  )
