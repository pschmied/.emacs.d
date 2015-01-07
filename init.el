;; Bootstrap init.el. Contains bare minimum to set up org-mode and
;; pull the remainder of my configuration from "settings.org"

;; Allow emacs to start a server so that EDITOR environment can be set
(server-start)

;; Bootstrap the remainder of our configuration
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
