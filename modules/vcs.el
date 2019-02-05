(defvar vcs-project-root '(".git" "package.json")
  "The list of files located in the project's root folder.
Used to determine whether its a root folder or not.")


(defun vcs-current-dir ()
  "Get buffer's directory based on its file name or default-directory if nill."
  (interactive)
  (if buffer-file-name
    (file-name-directory buffer-file-name)
    default-directory))

(defun vcs-parent-dir (dirname)
  (file-name-directory
    (directory-file-name dirname)))

(defun vcs-project-dir-p (dirname)
  (cl-loop for file in vcs-project-root
    thereis (file-exists-p
              (expand-file-name file dirname))))

(defun vcs-find-project-dir (dirname)
  (let ((dir dirname)
         (wasfound nil)
         (i 0))

    ;; @todo remove excess checks for same dirs (assume dirname == "/")
    (while (and (< i 7) (not wasfound) dir)
      (progn
        (if (vcs-project-dir-p dir)
          (setq wasfound t)
          (setq dir (vcs-parent-dir dir)))

        (setq i (1+ i))))

    (if wasfound dir dirname)))

(defun vcs-project-dir ()
  "Get project directory for the current buffer."
  (interactive)
  (vcs-find-project-dir
    (vcs-current-dir)))
