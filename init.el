;; Peter's init.el

;; Allow emacs to start a server so that EDITOR environment can be set
(server-start)

(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; Aspell
(if (file-exists-p "/usr/local/bin/aspell")
    (progn
      (setq ispell-program-name "aspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(melpa
                      exec-path-from-shell
                      starter-kit
                      starter-kit-bindings
                      guru-mode
                      paredit
                      ess
                      cider
                      clojure-mode
                      clojure-test-mode
                      paredit
                      auto-complete
                      twilight-theme
                      markdown-mode
                      markdown-mode+
                      pandoc-mode
                      auctex
                      haskell-mode
                      scion)
  "A list of packages to ensure are installed at launch.")



(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Guru mode to make me learn emacs keybindings
(require 'guru-mode)

;; Twilight theme enable
(load-theme 'twilight t)

;; Default font
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 130))

;; Set emacs default window size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1920)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 55)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(menu-bar-mode 1)

(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default)))
 '(sql-sqlite-program "/usr/local/bin/spatialite"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Compile LaTeX to PDF by default
(setq TeX-PDF-mode t)
(setq TeX-view-program-selection '((output-pdf "Skim")))

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; SLIME
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Paths, because they're broken on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ESS - Emacs Speaks Statistics
(require 'ess-site)
(setq inferior-julia-program-name "/usr/local/bin/julia-release-basic")

(add-hook 'clojure-mode-hook 'paredit-mode)

;; Python - Note: configuration assumes ipython is installed
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
