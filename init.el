;;; -*- lexical-binding: t -*-

(defvar mp-startup-time (current-time))
(defvar mp-init-time nil)

(defun mp-compute-startup-time ()
  (setq mp-init-time (float-time (time-subtract (current-time) mp-startup-time)))
  )

(add-hook 'after-init-hook 'mp-compute-startup-time)

(defun display-startup-echo-area-message ()
  (message "Initialized in %.3fs" mp-init-time)
  )

;; Install packages

(setq
 package-selected-packages
 '(
   company         ; Modular in-buffer completion framework
   company-tabnine ; TabNine completion backend
   cyphejor        ; Shorten names of major modes
   editorconfig    ; EditorConfig plugin
   f               ; Modern API for working with files and dirs
   fireplace
   flycheck
   js2-mode        ; Improved JavaScript editing mode
   ivy             ; A generic completion mechanism
   modalka         ; Native modal editing of your own design
   smart-mode-line ; A powerful and beautiful mode-line for Emacs
   smartparens     ; Tricks for working with all kinds of parenthesis
   swiper          ; Isearch with an overview
   use-package     ; Declarative package configuration
   yaml-mode       ; Major mode for editing YAML serialization format
   yasnippet       ; Yet another snippet extension for Emacs
   ztree           ; Show directory structure as a tree
   ))

(require 'package)
(require 'bytecomp)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (when (and (assq package package-archive-contents)
	     (not (package-installed-p package)))
    (package-install package t)))

;; Set up directories

(require 'f)

(defvar my-dir (f-expand "my" user-emacs-directory)
  "The directory where all the configuration files are kept.")

(add-to-list 'load-path my-dir)

;; Set up emacs

(require 'my-global)
(require 'my-packages)
