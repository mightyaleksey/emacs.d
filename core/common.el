;;; Package --- Summary
;;; Common settings and useful functions

(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)

  (setq-default line-spacing 2)
  (set-face-attribute 'default nil
    :family "Meslo LG S"
    :height 140
    :weight 'normal
    :width 'normal))


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving


(setq inhibit-startup-screen t)


;;; deal with whitespaces
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; common helpers

(defun dt-map-keys (@keymap-name @key-cmd-alist)
  "Define keys in a declarative way."
  (interactive)
  (mapc
    (lambda ($pair)
      (define-key @keymap-name (kbd (car $pair)) (cdr $pair)))
    @key-cmd-alist))

(defun insert-mdash ()
  "Insert medium dash symbol."
  (interactive)
  (insert-char 8212))


;;; project root directory lookup methods

(defvar dt-project-root '(".git" "package.json")
  "The list of files located in the project's root folder.
Used to determine whether its a root folder or not.")


(defun dt-current-dir ()
  "Get buffer's directory based on its file name or default-directory if nill."
  (interactive)
  (if buffer-file-name
    (file-name-directory buffer-file-name)
    default-directory))

(defun dt-parent-dir (dirname)
  (file-name-directory
    (directory-file-name dirname)))

(defun dt-project-dir-p (dirname)
  (cl-loop for file in dt-project-root
    thereis (file-exists-p
              (expand-file-name file dirname))))

(defun dt-find-project-dir (dirname)
  (let ((dir dirname)
         (wasfound nil)
         (i 0))

    ;; @todo remove excess checks for same dirs (assume dirname == "/")
    (while (and (< i 7) (not wasfound))
      (progn
        (if (dt-project-dir-p dir)
          (setq wasfound t)
          (setq dir (dt-parent-dir dir)))

        (setq i (1+ i))))

    (if wasfound dir dirname)))

(defun dt-project-dir ()
  "Get project directory for the current buffer."
  (interactive)
  (dt-find-project-dir
    (dt-current-dir)))
