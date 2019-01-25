;;; Package --- Summary
;;; tide

(add-to-list 'auto-mode-alist '("\\.tsx" . typescript-mode))

(use-package tide
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
          (typescript-mode . tide-hl-identifier-mode))
  :config
  (setq tide-node-executable "/usr/local/bin/node")

  )
