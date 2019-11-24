;;; -*- lexical-binding: t -*-

;; Some functions are borrower from "xah-fly-keys"
;; and renamed accoring to the local prefix "my-".

;; The original code of those functions was written by
;; Xah Lee and can be found at
;; https://github.com/xahlee/xah-fly-keys

(defun my-noop ()
  "Empty function that does nothing."
  (interactive)
  )

(defun my-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
	(re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

(defun my-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (progn
	(forward-line 1)
	(end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

(defun my-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.
when there's no selection,
• if cursor is on a any type of bracket (parenthesis, quote), select whole bracketed thing including bracket
• else, select current word.
when there's a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-09-01"
  (interactive)
  (if (region-active-p)
      (progn
	(let (($rb (region-beginning)) ($re (region-end)))
	  (goto-char $rb)
	  (cond
	   ((looking-at "\\s(")
	    (if (eq (nth 0 (syntax-ppss)) 0)
		(progn
		  ;; (message "left bracket, depth 0.")
		  (end-of-line) ; select current line
		  (set-mark (line-beginning-position)))
	      (progn
		;; (message "left bracket, depth not 0")
		(up-list -1 t t)
		(mark-sexp))))
	   ((eq $rb (line-beginning-position))
	    (progn
	      (goto-char $rb)
	      (let (($firstLineEndPos (line-end-position)))
		(cond
		 ((eq $re $firstLineEndPos)
		  (progn
		    ;; (message "exactly 1 line. extend to next whole line." )
		    (forward-line 1)
		    (end-of-line)))
		 ((< $re $firstLineEndPos)
		  (progn
		    ;; (message "less than 1 line. complete the line." )
		    (end-of-line)))
		 ((> $re $firstLineEndPos)
		  (progn
		    ;; (message "beginning of line, but end is greater than 1st end of line" )
		    (goto-char $re)
		    (if (eq (point) (line-end-position))
			(progn
			  ;; (message "exactly multiple lines" )
			  (forward-line 1)
			  (end-of-line))
		      (progn
			;; (message "multiple lines but end is not eol. make it so" )
			(goto-char $re)
			(end-of-line)))))
		 (t (error "logic error 42946" ))))))
	   ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
	    (progn
	      ;; (message "less than 1 line" )
	      (end-of-line) ; select current line
	      (set-mark (line-beginning-position))))
	   (t
	    ;; (message "last resort" )
	    nil))))
    (progn
      (cond
       ((looking-at "\\s(")
	;; (message "left bracket")
	(mark-sexp)) ; left bracket
       ((looking-at "\\s)")
	;; (message "right bracket")
	(backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
	;; (message "string quote")
	(mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
	;; (message "left is word or symbol")
	(skip-syntax-backward "_w" )
	;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
	(push-mark)
	(skip-syntax-forward "_w")
	(setq mark-active t)
	;; (exchange-point-and-mark)
	)
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
	;; (message "left and right both space" )
	(skip-chars-backward "\\s " ) (set-mark (point))
	(skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
	;; (message "left and right both newline")
	(skip-chars-forward "\n")
	(set-mark (point))
	(re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t
	;; (message "just mark sexp" )
	(mark-sexp)
	(exchange-point-and-mark))
       ;;
       ))))

(defun my-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: '\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, not the inner text of a bracket. For example, if text is
 (a(b)c▮)
 the selected char is “c”, not “a(b)c”.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2018-10-11"
  (interactive)
  (let (
	($skipChars "^'\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
	$p1
	)
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

(defun my-delete-blank-lines ()
  "Delete all newline around cursor.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02"
  (interactive)
  (let ($p3 $p4)
    (skip-chars-backward "\n")
    (setq $p3 (point))
    (skip-chars-forward "\n")
    (setq $p4 (point))
    (delete-region $p3 $p4)))

(defun my-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun my-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.
Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2019-06-13"
  (interactive)
  (let* (
	 ($eol-count 0)
	 ($p0 (point))
	 $p1 ; whitespace begin
	 $p2 ; whitespace end
	 ($charBefore (char-before))
	 ($charAfter (char-after ))
	 ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
	 $just-1-space-p
	 )
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
	  (my-fly-delete-spaces)
	(progn (my-fly-delete-spaces)
	       (insert " ")))
      )
     ((eq $eol-count 1)
      (if $space-neighbor-p
	  (my-fly-delete-spaces)
	(progn (my-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
	  (my-fly-delete-spaces)
	(progn
	  (my-delete-blank-lines)
	  (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
	  (my-fly-delete-spaces)
	(progn
	  (goto-char $p2)
	  (search-backward "\n" )
	  (delete-region $p1 (point))
	  (insert "\n"))))
     (t (progn
	  (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(defun my-insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (insert " "))

(defun my-insert-semicolon ()
  "Insert semicolon."
  (interactive)
  (insert ";"))

(defvar my-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")

(defvar my-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")

(defun my-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `my-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt my-left-brackets) nil t))

(defun my-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `my-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt my-right-brackets) nil t))

(defvar my-recently-closed-buffers nil
  "alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `my-recently-closed-buffers-max'.")

(defvar my-recently-closed-buffers-max 40
  "The maximum length for `my-recently-closed-buffers'.")

(defun my-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version 2016-06-18"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   (t t)))

(defun my-close-current-buffer ()
  "Close the current buffer.
Similar to `kill-buffer', with the following addition:
• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2018-06-11"
  (interactive)
  (let (($org-p (string-match "^*Org Src" (buffer-name))))
    (if (string= major-mode "minibuffer-inactive-mode")
	(minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
	;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
	(when (and (buffer-modified-p)
		   (my-user-buffer-q)
		   (not (string-equal major-mode "dired-mode"))
		   (if (equal (buffer-file-name) nil)
		       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
		     t))
	  (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
	      (save-buffer)
	    (set-buffer-modified-p nil)))
	(when (and (buffer-modified-p)
		   $org-p)
	  (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
	      (org-edit-src-save)
	    (set-buffer-modified-p nil)))
	;; save to a list of closed buffer
	(when (buffer-file-name)
	  (setq my-recently-closed-buffers
		(cons (cons (buffer-name) (buffer-file-name)) my-recently-closed-buffers))
	  (when (> (length my-recently-closed-buffers) my-recently-closed-buffers-max)
	    (setq my-recently-closed-buffers (butlast my-recently-closed-buffers 1))))
	(kill-buffer (current-buffer))))))

(defun my-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
It returns the buffer (for elisp programing).
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(defun my-open-last-closed ()
  "Open the last closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (if (> (length my-recently-closed-buffers) 0)
      (find-file (cdr (pop my-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(provide 'my-utils)
