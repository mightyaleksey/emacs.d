(defvar keys-mode-activate-hook nil
  "Hook for `keys` minor mode.")

(defvar keys-mode-map (make-sparse-keymap)
  "Keybinding for `keys` minor mode.")

(defvar keys-command-keys-alist
  '(
    ("1" . keys-noop)
    ("2" . keys-noop)
    ("3" . keys-noop)
    ("4" . keys-noop)
    ("5" . keys-noop)
    ("6" . keys-noop)
    ("7" . keys-noop)
    ("8" . keys-noop)
    ("9" . keys-noop)
    ("0" . keys-noop)
    ("-" . keys-noop)
    ("=" . keys-noop)

    ("q" . keys-noop)
    ("w" . keys-noop)
    ("e" . keys-noop)
    ("r" . keys-noop)
    ("t" . keys-noop)
    ("y" . keys-noop)
    ("u" . keys-noop)
    ("i" . keys-noop)
    ("o" . keys-noop)
    ("p" . keys-noop)
    ("[" . keys-noop)
    ("]" . keys-noop)

    ("a" . keys-noop)
    ("s" . keys-noop)
    ("d" . keys-noop)
    ("f" . keys-noop)
    ("g" . keys-noop)
    ("h" . keys-noop)
    ("j" . keys-noop)
    ("k" . keys-noop)
    ("l" . keys-noop)
    (";" . keys-noop)
    ("'" . keys-noop)
    ("\\" . keys-noop)

    ("z" . keys-noop)
    ("x" . keys-noop)
    ("c" . keys-noop)
    ("v" . keys-noop)
    ("b" . keys-noop)
    ("n" . keys-noop)
    ("m" . keys-noop)
    ("," . keys-noop)
    ("." . keys-noop)
    ("/" . keys-noop)

    (" " . keys-noop)
    )
  "Command mode keybindings.")

(defvar keys-alias-alist
  '(
    ("q" . "й")
    ("w" . "ц")
    ("e" . "у")
    ("r" . "к")
    ("t" . "е")
    ("y" . "н")
    ("u" . "г")
    ("i" . "ш")
    ("o" . "щ")
    ("p" . "з")
    ("[" . "х")
    ("]" . "ъ")

    ("a" . "ф")
    ("s" . "ы")
    ("d" . "в")
    ("f" . "а")
    ("g" . "п")
    ("h" . "р")
    ("j" . "о")
    ("k" . "л")
    ("l" . "д")
    (";" . "ж")
    ("'" . "э")
    ("\\" . "ё")

    ("z" . "я")
    ("x" . "ч")
    ("c" . "с")
    ("v" . "м")
    ("b" . "и")
    ("n" . "т")
    ("m" . "ь")
    ("," . "б")
    ("." . "ю"))
  "List of key aliases to support different keyboard layouts.")


(defun keys-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (mapc
   (lambda (p)
     (let* ((key (car p))
            (alias (assoc key keys-alias-alist))
            (fn (cdr p)))
       (define-key keys-mode-map key fn)
       (if alias (define-key keys-mode-map (cdr alias) fn))))
   keys-command-keys-alist
   )

  (setq mode-line-front-space "C")
  (force-mode-line-update)
  )

(defun keys-insertion-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (mapc
   (lambda (p)
     (let* ((key (car p))
            (alias (assoc key keys-alias-alist)))
       (define-key keys-mode-map key nil)
       (if alias (define-key keys-mode-map (cdr alias) nil))))
   keys-command-keys-alist
   )

  (setq mode-line-front-space "I")
  (force-mode-line-update)
  )

(defun keys-define (key fn)
  (let ((pair (assoc key keys-command-keys-alist)))
    (if pair (setcdr pair fn)))
  )

(defun keys-noop ()
  "Empty function which does nothing."
  (interactive)
  )


(defvar keys-action-alist nil 
  "Maps action names to corresponding functions based on major mode.")

(defun keys-action-define (action-name mode-name fn)
  (let* ((key (format "%s--%s" action-name mode-name))
         (pair (assoc key keys-action-alist)))

    (if pair
        (setcdr pair fn)
      (add-to-list 'keys-action-alist (cons key fn))
      ))
  )

(defun keys-action-call (action-name)
  (let* ((key (format "%s--%s" action-name major-mode))
         (df-key (format "%s--%s" action-name nil))
         (fn (or
              (assoc key keys-action-alist)
              (assoc df-key keys-action-alist))))

    (if fn (call-interactively (cdr fn) nil)))
  )


(define-minor-mode keys-mode
  "Modal keybindings set. Inspired by `xah-fly-keys`."
  t "χkeys" keys-mode-map
  
  (add-hook 'minibuffer-setup-hook 'keys-insertion-mode-activate)
  (add-hook 'minibuffer-exit-hook 'keys-command-mode-activate)

  (keys-command-mode-activate)

  (run-hooks 'keys-mode-activate-hook)
)

(provide 'core-keys)
