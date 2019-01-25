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
