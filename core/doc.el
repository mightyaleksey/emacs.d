;;; Package --- Summary
;;; Provide documentation in a popup instead of opening another window.

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
