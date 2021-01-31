(define-key global-map (kbd "C-i")
  '(lambda () (interactive)(message "pressed ctrl-i"))) ;; tab
(define-key global-map (kbd "C-m")
  '(lambda () (interactive)(message "pressed ctrl-m"))) ;; enter


(defun test-seq (key msg)
  (define-key global-map (kbd key)
    `(lambda ()
      (interactive)
      (message "pressed %s" ,msg))))


(test-seq "s-w" "cmd-w")
(test-seq "s-e" "cmd-e")
(test-seq "s-r" "cmd-r")
(test-seq "s-t" "cmd-t")
(test-seq "s-y" "cmd-y")
(test-seq "s-u" "cmd-u")
(test-seq "s-i" "cmd-i")
(test-seq "s-o" "cmd-o")
(test-seq "s-p" "cmd-p")
(test-seq "s-[" "cmd-[")
(test-seq "s-]" "cmd-]")

(test-seq "s-a" "cmd-a")
(test-seq "s-s" "cmd-s")
(test-seq "s-d" "cmd-d")
(test-seq "s-f" "cmd-f")
(test-seq "s-g" "cmd-g")
(test-seq "s-h" "cmd-h")
(test-seq "s-j" "cmd-j")
(test-seq "s-k" "cmd-k")
(test-seq "s-l" "cmd-l")
(test-seq "s-;" "cmd-;")
(test-seq "s-'" "cmd-'")

(test-seq "s-z" "cmd-z")
(test-seq "s-x" "cmd-x")
(test-seq "s-c" "cmd-c")
(test-seq "s-v" "cmd-v")
(test-seq "s-b" "cmd-b")
(test-seq "s-n" "cmd-n")
(test-seq "s-m" "cmd-m")
(test-seq "s-," "cmd-,")
(test-seq "s-." "cmd-.")
(test-seq "s-/" "cmd-/")

(test-seq "s-K" "cmd-shift-k")
