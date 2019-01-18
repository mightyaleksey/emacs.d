;;; Package --- Summary
;;; ivy

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
