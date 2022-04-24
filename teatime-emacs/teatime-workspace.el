;; -*- lexical-binding: t -*-

;; vscode workspaces https://code.visualstudio.com/docs/editor/workspaces
(require 'teatime-fsearch)

(defvar teatime-workspace--folders nil
  "List of folders in the current workspace.")
(defvar teatime-workspace--ignore-buffers '(teatime-fsearch--search-buffer-name)
  "List of buffer names in which 'default-directory'
should not be updated.")

;;
;; p u b l i c   f u n c t i o n s
;;

(defun teatime-workspace-add-folder (folder)
  "Adds folder to the workspace."
  (add-to-list 'teatime-workspace--folders folder))

(defun teatime-workspace-get-folder-for-file (&optional filename)
  "Finds matching workspace folder for a file,
otherwise returns directory of file."
  (when filename
    (let ((folder nil))

      (dolist (current-folder teatime-workspace--folders)
        (when (string-prefix-p current-folder filename)
          ;; todo early break
          (setq folder current-folder)))

      (file-name-as-directory
        (or folder
            (file-name-directory filename))))))


(defun teatime-workspace--update-directory-hook ()
  (unless (member (buffer-name) teatime-workspace--ignore-buffers)
    (message (format "update %s" (buffer-name)))
    (let ((folder (or (teatime-workspace-get-folder-for-file (buffer-file-name))
                      (car teatime-workspace--folders))))

      (when folder
        (setq-local default-directory folder)))))

(define-minor-mode teatime-workspace-mode
  "Toggles teatime workspace mode."
  nil
  :global t
  (if teatime-workspace-mode
      (add-hook 'after-change-major-mode-hook 'teatime-workspace--update-directory-hook)
    (remove-hook 'after-change-major-mode-hook 'teatime-workspace--update-directory-hook)))

(provide 'teatime-workspace)
