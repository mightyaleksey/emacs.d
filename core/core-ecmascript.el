;;; todo
;; add 'x.log' action which makes replacement to 'console.log(x)'

;;; https://www.emacswiki.org/emacs/ModeTutorial
(defvar es-mode-hook nil
  "Hook run when entering EcmaScript mode.")

(defvar es-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for EcmaScript major mode.")

(defcustom es-indent-level 2
  "Number of spaces for each indentation step in 'es-mode'."
  :type 'integer
  :safe 'integerp
  :group 'es)


(defvar es--font-lock-keywords-1
  (concat "\\<" (regexp-opt '("const" "let" "var" "class" "function") t) "\\>")
  "Definitions")

(defvar es--font-lock-keywords-2
  (concat "\\<" (regexp-opt '("false" "true" "null" "undefined") t) "\\>")
  "Primitives")

(defvar es-font-lock-keywords
  (list
   (cons es--font-lock-keywords-1 font-lock-builtin-face)
   (cons es--font-lock-keywords-2 font-lock-constant-face)
   '("\\<\\(\\w+\\)(" 1 font-lock-function-name-face)
   )
  "Default highlighting expressions for EcmaScript mode.")


(defvar es--forward-indent "\\(\\sw\\s-*?`\\|{\\)\\s-*?$")
(defvar es--backward-indent "^\\s-*?[}`]")

(defun es--match-line (pattern &optional low-limit high-limit)
  (let ((beginning (line-beginning-position)) 
        (ending (line-end-position)))

    (cond ((numberp low-limit)
           (setq ending (min ending (+ beginning low-limit))))
          ((numberp high-limit)
           (setq beginning (max beginning (- ending high-limit)))))

    (string-match pattern
                  (buffer-substring-no-properties beginning ending))
    ))

(defun es-indent-line ()
  "Indent current line as EcmaScript code."
  (interactive)
  (let ((column (current-column))
        (prev-indent nil)
        (cur-indent (current-indentation))
        (delta 0))

    (if (> (line-number-at-pos) 1) 
        (progn
          (forward-line -1)
          (setq prev-indent (current-indentation))
          (if (es--match-line es--forward-indent nil 120)
              (setq prev-indent (+ prev-indent es-indent-level)))
          (forward-line 1)
          (if (es--match-line es--backward-indent 240)
              (setq prev-indent (- prev-indent es-indent-level)))
          ))

    (if (and prev-indent (> prev-indent cur-indent))
        (setq delta (- prev-indent cur-indent))
      (setq delta es-indent-level))

    (back-to-indentation)
    (indent-to (+ cur-indent delta))

    (unless (> cur-indent column)
      (move-to-column (+ column delta)))
    ))


(defvar es-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?$ "w" st)

    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?` "\"" st)

    (modify-syntax-entry ?\/ ". 124" st)
    (modify-syntax-entry ?\* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for EcmaScript mode.")


;; Imenu
(defvar es-imenu-syntax-alist
  '(("_$" . "w"))
  )

(defvar es-imenu-generic-expression
  '((nil "\\_<\\(module\\)\\>" 1)
    ("Classes" "class +\\_<\\(\\w+\\)\\_>" 1)
    ("Functions" "function +\\_<\\(\\w+\\)\\_>" 1)
    ("Variables" "\\(const\\|let\\|var\\) +\\_<\\(\\w+\\)\\_>" 2))
  )


(defun es-mode ()
  "Major mode for editing EcmaScript files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table es-mode-syntax-table)
  (use-local-map es-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(es-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'es-indent-line)
  (setq major-mode 'es-mode)
  (setq mode-name "εs")

  (setq imenu-syntax-alist es-imenu-syntax-alist)
  (setq imenu-generic-expression es-imenu-generic-expression)

  ;; comment-line support
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (run-hooks 'es-mode-hook)
  )


;; Node.js repl
(require 'subr-x)

(defvar es-nodejs-process-name "node"
  "Process name of Node.js REPL.")

(defun es-nodejs-version ()
  "Print Node.js version"
  (interactive)
  (message
   (string-trim-right
    (with-output-to-string
      (with-current-buffer
          standard-output
        (call-process es-nodejs-process-name nil t nil "-v")))))
  )

(defun es--run-code (input)
  (string-trim-right
   (with-output-to-string
     (with-current-buffer
         standard-output
       (call-process es-nodejs-process-name nil t nil "-p" input))))
  )

(defun es-nodejs-eval-buffer ()
  "Evaluate current buffer and print value in the echo area."
  (interactive)
  (if (> (buffer-size) 0)
      (message (es--run-code (buffer-string))))
  )

(defun es-nodejs-eval-line ()
  "Evaluate current line and print value in the echo area."
  (interactive)
  (message (es--run-code (thing-at-point 'line)))
  )

(defun es-nodejs-eval-region ()
  (interactive)
  (if mark-active
      (message (es--run-code (buffer-substring (region-beginning) (region-end))))
    )
  )


(provide 'core-ecmascript)
