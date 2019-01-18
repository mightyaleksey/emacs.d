;;; Package --- Summary
;;; tide

(use-package tide
  :defer t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
          (typescript-mode . tide-hl-identifier-mode)))
