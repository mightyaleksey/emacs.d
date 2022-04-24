;; -*- lexical-binding: t -*-

;; adds path to teatime emacs modules
(add-to-list 'load-path (file-name-directory
                         (or load-file-name buffer-file-name)))

(require 'ert)

(require 'teatime-fsearch)

(ert-deftest teatime-fsearch--get-command ()
  (setq teatime-fsearch-ignore-patterns nil)

  (should (equal
	   (teatime-fsearch--get-command "init.el" "/tmp/")
	   "cd /tmp/ && find . -iwholename '*i*n*i*t*.*e*l*' -type f  | head -10 | cut -c3-"))

  (setq teatime-fsearch-ignore-patterns
	'("*/.git/*" "*/.hg/*" "*/node_modules/*"))

  (should (equal
	   (teatime-fsearch--get-command "init.el" "/tmp/")
	   "cd /tmp/ && find . -iwholename '*i*n*i*t*.*e*l*' -type f -not \\( -path '*/.git/*' -o -path '*/.hg/*' -o -path '*/node_modules/*' \\) | head -10 | cut -c3-")))


(require 'teatime-workspace)

(ert-deftest teatime-workspace-add-folder ()
  "Should add folder to list even if it nil."
  (teatime-workspace-add-folder user-emacs-directory)

  (should (equal
	   teatime-workspace--folders
	   (list user-emacs-directory)))

  ;; clean up
  (setq teatime-workspace--folders nil))

(ert-deftest teatime-workspace-get-folder-for-file ()
  (should (equal
	   (teatime-workspace-get-folder-for-file nil)
	   nil))

  (should (equal
	   (teatime-workspace-get-folder-for-file "tmp/teatime/teatime.el")
	   "tmp/teatime/"))

  (teatime-workspace-add-folder "tmp")

  (should (equal
	   (teatime-workspace-get-folder-for-file "tmp/teatime/teatime.el")
	   "tmp/"))

  ;; clean up
  (setq teatime-workspace--folders nil))
