;; -*- lexical-binding: t -*-

(defun teatime-theme-mode-line-space-between (left right)
  (let ((available-width (- (window-total-width)
                            (+ (length (format-mode-line left))
                               (length (format-mode-line right))))))

    (list
     left
     (make-string available-width (string-to-char " "))
     right)))

;; show UTF-8 instead of U
;; coding-system-alist - available coding system names.
(defvar teatime-theme-mode-line-encoding
  '(:eval (upcase
           (replace-regexp-in-string "-\\(dos\\|mac\\|unix\\)" ""
                                     (symbol-name buffer-file-coding-system)))))

(put 'teatime-theme-mode-line-encoding 'risky-local-variable t)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html#Mode-Line-Variables
;; https://github.com/kgaipal/emacs/blob/master/lisp/mode-line-customization.el
(setq-default mode-line-format
              '((:eval (teatime-theme-mode-line-space-between
                        (format-mode-line (list
                                           " "
                                           (propertize "%b" 'face 'mode-line-buffer-id)))
                        (format-mode-line (list
                                           "Ln %l, Col %c"
                                           "   "
                                           'teatime-theme-mode-line-encoding
                                           "   "
                                           'mode-name " " 'minor-mode-alist
                                           " "))))))

(provide 'teatime-theme)
