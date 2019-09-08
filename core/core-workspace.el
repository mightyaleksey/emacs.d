;; add support for editorconfig
(defvar workspace-project-directory nil
  "Project root directory.")

(defvar workspace-project-root '(".git")
  "List of files located in the project's root folder. 
Used to determine the project root.")

(defun workspace--project-root-p (directory)
  (cl-loop for file in workspace-project-root
           thereis (file-exists-p
                    (expand-file-name file directory)))
  )

(defun workspace--get-buffer-directory ()
  "Get directory of the current buffer."
  (if buffer-file-name 
      (file-name-directory buffer-file-name))
  )

(defun workspace--lookup-directory (&optional directory)
  "Find project directory from provided `directory`."
  (let ((found nil)
        (current-directory (or directory default-directory)))

    (while (and (not found) current-directory)
      (if (workspace--project-root-p current-directory) 
          (setq found t)
        (setq current-directory
              (file-name-directory (directory-file-name current-directory)))))

    (if found 
        current-directory 
      directory)
    ))

(defun workspace-get-directory ()
  "Get project directory for current buffer."
  (interactive)
  (let ((buffer-directory (workspace--get-buffer-directory)))
    
    (if (not (and 
              workspace-project-directory 
              (string-prefix-p workspace-project-directory buffer-directory)))
        (setq workspace-project-directory (workspace--lookup-directory)))

    (or workspace-project-directory buffer-directory default-directory)
    ))

(defun workspace-reset-directory ()
  "Reset project directory."
  (interactive)
  (setq workspace-project-directory nil)
  )


;; (define-minor-mode workspace-editorconfig-mode
;;   "Editorconfig support."
;;   t "εconfig" nil
;;   ;; change-major-mode-after-body-hook
;;   ;; load config
;;   ;; create alist of regexps and corresponding params
;;   ;; match buffer-file-name with params and apply them
;; )


(provide 'core-workspace)
