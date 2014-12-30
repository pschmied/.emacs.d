;; Bootstrap init.el. Contains bare minimum to set up org-mode and
;; pull the remainder of my configuration from "settings.org"

;; Allow emacs to start a server so that EDITOR environment can be set
(server-start)

;; Set up the melpa package repository
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install packages required for bootstrap (probably just org)
(defvar bootstrap-packages '(org) "Packages required for bootstrap")
(dolist (p bootstrap-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Bootstrap the remainder of our configuration
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
