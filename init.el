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

(defvar my-packages '(exec-path-from-shell
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
                      scion
                      org
                      org-dotemacs
                      ox-pandoc
                      polymode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Markdown
(add-hook 'markdown-mode-hook 'pandoc-mode)   ; pandoc by default
(add-hook 'markdown-mode-hook 'flyspell-mode) ; spell checker

;; Orgmode
(setq initial-major-mode 'org-mode)

;; active Org Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   ;(julia . t)
   (sh . t)
   (latex . t)
   (python . t)
   (sql . t)
   (sqlite . t)))

;; Org babel inline images
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)

;; Guru mode to make me learn emacs keybindings
(require 'guru-mode)

;; Twilight theme enable
;; (load-theme 'twilight t)
(load-theme 'leuven t)                  ; Maybe leuven

;; Default font
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Source Code Pro" :height 130))

;; Set emacs default window size
;; NOTE: Nixing this due to adoption of Amethyst for OSX
;;
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if (display-graphic-p)
;;   (progn
;;     ;; use 120 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1920)
;;            (add-to-list 'default-frame-alist (cons 'width 120))
;;            (add-to-list 'default-frame-alist (cons 'width 80)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;          (cons 'height (/ (- (x-display-pixel-height) 55)
;;                              (frame-char-height)))))))

;; (set-frame-size-according-to-resolution)

(menu-bar-mode 1)

(setq ring-bell-function 'ignore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latexmk -pdf")
 '(TeX-PDF-mode t)
 '(TeX-command-list
   (quote
    (("LaTeXmk" "latexmk -pdf %t" TeX-run-command nil t)
     ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   (quote
    ("e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default)))
 '(doc-view-continuous t)
 '(ess-swv-processor (quote knitr))
 '(org-agenda-files nil)
 '(show-paren-mode t)
 '(sql-sqlite-program "/usr/local/bin/spatialite")
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; LaTeX config
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)
;; Compile LaTeX to PDF by default
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LaTeXmk")))
(add-hook 'LaTeX-mode-hook '(lambda () (setq compile-command "latexmk -pdf")))


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
(setq inferior-julia-program-name "/opt/homebrew-cask/Caskroom/julia/0.3.2/Julia-0.3.2.app/Contents/Resources/julia/bin/julia")

;; Paredit when clojuring
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Change overlap when paging up and down
(setq next-screen-context-lines 4)

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


;; A function to insert date
(defun esk-insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))


;; Mouse scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time



;; Email
;; (require 'mu4e)
;; (require 'smtpmail)

;; (setq mu4e-maildir "~/Mail")

;; (setq mu4e-drafts-folder "/Personal/[Gmail].Drafts"
;;       mu4e-sent-folder   "/Personal/[Gmail].Sent Mail"
;;       mu4e-trash-folder  "/Personal/[Gmail].Trash"
;;       mu4e-sent-messages-behavior 'delete
;;       mu4e-get-mail-command "offlineimap"
;;       mu4e-update-interval 60
;;       user-mail-address "user@example.com"
;;       user-full-name  "Full Name"
;;       mu4e-maildir-shortcuts
;;       '( ("/Personal/INBOX"               . ?i)
;;          ("/Personal/[Gmail].Sent Mail"   . ?s)
;;          ("/Personal/[Gmail].Trash"       . ?t)
;;          ("/Personal/[Gmail].All Mail"    . ?a)
;;          ("/UW/INBOX"                     . ?k)
;;          ("/UW/Sent Items"                . ?x)
;;          ("/UW/Trash"                     . ?f))

;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-stream-type 'starttls
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       message-kill-buffer-on-exit t)

;; ;; define only variables that differ for the two accounts
;; (defvar my-mu4e-account-alist
;;   '(("Gmail"
;;      (mu4e-drafts-folder "/Personal/[Gmail].Drafts")
;;      (mu4e-sent-folder   "/Personal/[Gmail].Sent Mail")
;;      (mu4e-trash-folder  "/Personal/[Gmail].Trash")
;;      (user-mail-address  "@.net")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587))
;;     ("UW"
;;      (mu4e-drafts-folder "/UW/[Gmail].Drafts")
;;      (mu4e-sent-folder   "/UW/[Gmail].Sent Items")
;;      (mu4e-trash-folder  "/UW/[Gmail].Trash")
;;      (user-mail-address  "@uw.edu")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-smtp-service 587))))

;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                              nil t nil nil (caar my-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       (error "No email account found"))))

;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; (setq mu4e-view-show-images t)
;; (when (fboundp 'imagemagick-register-types)
;;   (imagemagick-register-types))
;; (setq mu4e-view-prefer-html t)
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")
;; (setq mail-user-agent 'mu4e-user-agent)
