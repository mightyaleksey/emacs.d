(use-package js
  :defer t

  :init
  (setq-default js-indent-align-list-continuation nil)

  :config
  (defun js--multi-line-declaration-indentation ()
    "Override the default function `js--multi-line-declaration-indentation',
which is a helper for the multiline variable declaration.
The default behavior spoils indentation for the beggining of chain
on the second line in expression. Since usage of multiline variable declaration
is unpopular nowadays (it produces additional diffs in case you need to add or remove smthing),
turn off indentation support for it."
    nil))

(use-package rjsx-mode
  :defer t

  :bind
  (:map rjsx-mode-map
    ("<"   . nil)
    ("C-d" . nil)
    (">"   . nil))

  :mode ("\\.js" . rjsx-mode))
