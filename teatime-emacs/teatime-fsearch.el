;; -*- lexical-binding: t -*-

(require 'subr-x)

(defgroup teatime-fsearch nil ""
  :group 'teatime)

(defcustom teatime-fsearch-ignore-patterns
  '("*/.git/*"
    "*/.hg/*"
    "*/node_modules/*")
  "List of patterns to exclude from search."
  :group 'teatime-fsearch)

(defface teatime-fsearch-directory-name
  '((t :inherit shadow :extend t))
  "Default face for highlighting directory name."
  :group 'teatime-fsearch)

(defface teatime-fsearch-selected-file
  '((t :inherit region :extend t))
  "Default face for highlighting selected file."
  :group 'teatime-fsearch)


(defvar teatime-fsearch--search-buffer-name "*Search files by name*"
  "Stores default buffer name for the search window.")
(defvar teatime-fsearch--search-results nil
  "Stores temporary search results.")

(defvar teatime-fsearch--current-window nil
  "Window to store search results in.")
(defvar teatime-fsearch--search-timer-id nil
  "Tracks scheduled 'teatime-fsearch--search' function call.")

(defvar teatime-fsearch--selected-file-index 0
  "Position of the selected file in 'teatime-fsearch--search-results' list.")
(defvar teatime-fsearch--selection-overlay nil
  "The overlay which is used to draw selection.")

;;
;; h e l p e r s
;;

(defun teatime-fsearch--get-command (needle directory)
  (let ((pattern "*")
        (ignore-patterns ""))

    ;; convert "abc" to "*a*b*c*"
    (dotimes (i (length needle))
      (setq pattern (concat
                     pattern
                     (char-to-string (aref needle i)) "*")))

    ;; generate "-not ( -path 'a' -or -path 'b' )"
    ;; from 'teatime-fsearch-ignore-patterns' list
    (when (> (length teatime-fsearch-ignore-patterns) 0)
      (setq ignore-patterns (format "-not \\( %s \\)"
				    (mapconcat
				     (lambda (p) (format "-path '%s'" p))
				     teatime-fsearch-ignore-patterns
				     " -o "))))

    (format
     "cd %s && find . -iwholename '%s' -type f %s | head -10 | cut -c3-"
     directory
     pattern
     ignore-patterns)))

(defun teatime-fsearch--exec-command (needle directory)
  ;; long invocation blocks I/O
  ;; add timeout, do async
  (mapcar
   (lambda (file)
     (concat (file-name-as-directory directory) file))
   (split-string
    (shell-command-to-string
     (teatime-fsearch--get-command needle directory)) "\n" t)))


(defun teatime-fsearch--get-search-string ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (point-min) (line-end-position))))

(defun teatime-fsearch--search-timer-remove ()
  (when teatime-fsearch--search-timer-id
    (cancel-timer teatime-fsearch--search-timer-id))
  (setq teatime-fsearch--search-timer-id nil))

(defun teatime-fsearch--search-hook (beginning end removed-length)
  (when (or (> (- end beginning) 0)
            (> removed-length 0))

    ;; debounce 'teatime-fsearch--search' invocation
    (teatime-fsearch--search-timer-remove)
    (setq teatime-fsearch--search-timer-id
	  (run-with-timer 0.2 nil 'teatime-fsearch--search))))

(defun teatime-fsearch--search ()
  (teatime-fsearch--search-timer-remove)

  (with-current-buffer teatime-fsearch--search-buffer-name
    (setq teatime-fsearch--search-results
	  (teatime-fsearch--exec-command
           (teatime-fsearch--get-search-string)
           default-directory)))

  ;; fix selected file index in case of overflow
  (when (>= teatime-fsearch--selected-file-index
            (length teatime-fsearch--search-results))

    (setq teatime-fsearch--selected-file-index
	  (max (1- (length teatime-fsearch--search-results)) 0)))

  (teatime-fsearch--render-search-results)
  (teatime-fsearch--render-selection-overlay))


(defun teatime-fsearch--update-selected-file-index (index offset total)
  (if (> total 0)
      (% (+ index offset total) total)
    0))


(defun teatime-fsearch--format-search-results (files directory)
  (if (= (length files) 0)
      "No matching results\n"
    (let ((str "")
          (dir ""))

      (dolist (file files str)
        (setq dir (directory-file-name (string-remove-prefix
                                        directory
                                        (or (file-name-directory file) ""))))
        (setq str (concat str (format
                               " ≡ %s %s\n"
                               (file-name-nondirectory file)
                               (propertize dir 'face 'teatime-fsearch-directory-name))))))))

(defun teatime-fsearch--render-search-results ()
  (let ((str (teatime-fsearch--format-search-results
              teatime-fsearch--search-results
              default-directory))
        (lines (1+ (teatime--count-lines-after-position (point-min)))))

    (save-excursion
      (when (= lines 1)
        (end-of-line)
        (insert "\n"))

      (goto-char (point-min))
      (forward-line)
      (delete-region (line-beginning-position) (point-max))
      (insert str))))

(defun teatime-fsearch--render-selection-overlay ()
  (unless (overlayp teatime-fsearch--selection-overlay)
    (setq teatime-fsearch--selection-overlay
	  (make-overlay (point) (point)))
    (overlay-put teatime-fsearch--selection-overlay
		 'face 'teatime-fsearch-selected-file))

  (if (> (length teatime-fsearch--search-results) 0)
      (save-excursion
        (goto-char (point-min))
        (forward-line (+ teatime-fsearch--selected-file-index 1))
        (move-overlay teatime-fsearch--selection-overlay
		      (line-beginning-position) (line-beginning-position 2)))
    (move-overlay teatime-fsearch--selection-overlay
		  (point-min) (point-min))))


(defvar teatime-fsearch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'teatime-fsearch-select-previous)
    (define-key map (kbd "<down>") 'teatime-fsearch-select-next)
    (define-key map (kbd "<escape>") 'teatime-fsearch-quit)
    (define-key map (kbd "RET") 'teatime-fsearch-open-selected-file)
    (define-key map (kbd "C-g") 'teatime-fsearch-quit)
    map)
  "Keymap for 'teatime-fsearch-mode'.")

(define-minor-mode teatime-fsearch-mode
  "Minor mode for the file search buffer."
  nil nil teatime-fsearch-mode-map)

;;
;; p u b l i c   f u n c t i o n s
;;

(defun teatime-fsearch-select-next ()
  "Select next file from search results."
  (interactive)

  (setq teatime-fsearch--selected-file-index
	(teatime-fsearch--update-selected-file-index
	 teatime-fsearch--selected-file-index
	 1
	 (length teatime-fsearch--search-results)))

  (teatime-fsearch--render-selection-overlay))

(defun teatime-fsearch-select-previous ()
  "Select previous file from search results."
  (interactive)

  (setq teatime-fsearch--selected-file-index
	(teatime-fsearch--update-selected-file-index
	 teatime-fsearch--selected-file-index
	 -1
	 (length teatime-fsearch--search-results)))

  (teatime-fsearch--render-selection-overlay))

(defun teatime-fsearch-open-selected-file ()
  "Opens selected file from the list."
  (interactive)

  (when (> (length teatime-fsearch--search-results) 0)
    (teatime-fsearch-quit)
    (switch-to-buffer (find-file
		       (nth teatime-fsearch--selected-file-index
			    teatime-fsearch--search-results)))))

(defun teatime-fsearch-quit ()
  "Close file search."
  (interactive)

  (when teatime-fsearch--current-window
    (select-window teatime-fsearch--current-window)
    (switch-to-buffer
     (get-buffer-create teatime-fsearch--search-buffer-name))

    ;; clean up
    (teatime-fsearch--search-timer-remove)
    (remove-hook 'after-change-functions 'teatime-fsearch--search-hook)
    (setq teatime-fsearch--current-window nil)

    (kill-buffer-and-window)))


(defun teatime-fsearch (&optional directory)
  "Performs file search."
  (interactive)

  (unless directory
    (if default-directory
        (setq directory default-directory)
      (user-error "No directory")))

  (if (not teatime-fsearch--current-window)
      (progn
        (setq teatime-fsearch--current-window
              (split-window (frame-root-window) -11 'above))

        (select-window teatime-fsearch--current-window)
        (switch-to-buffer
         (get-buffer-create teatime-fsearch--search-buffer-name))

        (with-current-buffer teatime-fsearch--search-buffer-name
          (setq default-directory (file-name-as-directory directory)))

        (linum-mode 0)
        (visual-line-mode 0)
        (teatime-fsearch-mode 1)

        (setq-local mode-line-format nil)
        ;; (setq-local mode-line-format '((:eval
        ;;   (propertize (make-string (window-total-width) 9472) 'face 'default))))

        (teatime-fsearch--render-search-results)
        (teatime-fsearch--render-selection-overlay)

        (add-hook 'after-change-functions 'teatime-fsearch--search-hook nil t))

    (progn
      (set-mark (point-min))
      (goto-char (point-min))
      (goto-char (line-end-position)))))

(provide 'teatime-fsearch)
