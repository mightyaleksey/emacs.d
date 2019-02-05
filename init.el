;;; measure load time
(setq startup-time (current-time))

(add-hook 'after-init-hook
	(lambda () (setq elapsed-time (float-time (time-subtract (current-time) startup-time)))))

(defun display-startup-echo-area-message ()
  (message "Loaded in %.3fs" elapsed-time))


;;; setup use-package
(require 'package)

(setq package-archives
  '(
     ("gnu"          . "http://elpa.gnu.org/packages/")
	   ("melpa"        . "http://melpa.org/packages/")
	   ("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-archive-priorities
  '(
     ("gnu"          . 0)
	   ("melpa"        . 10)
	   ("melpa-stable" . 5)))
(setq package-enable-at-startup nil)
(package-initialize nil)

(defun refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove 'package-install 'refresh-contents))

(advice-add 'package-install :before 'refresh-contents)

(unless (package-installed-p 'use-package)
  (message "EMACS install use-package.el")
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;;; add helper to load modules
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory) user-emacs-directory)
	  ((boundp 'user-init-directory) user-init-directory)
	  (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


(load-user-file "modules/ui.el")
(load-user-file "modules/vcs.el")
(load-user-file "modules/navigation.el")
(load-user-file "modules/helpers.el")
(load-user-file "modules/syntax.el")


(setq custom-file "~/.emacs.d/customize.el") ; configuration setted up through the customize menu
(load-user-file "customize.el")
