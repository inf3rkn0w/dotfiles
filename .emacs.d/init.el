(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ; Auto-install packages

(setq inhibit-startup-message t)
(setq inhibit-scratch-message nil) ;; Change with initial-scratch-message

;; Window setup
(use-package desktop)
;;(desktop-save-mode 1)
(scroll-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Exempt line numbers modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Typefacing/Display
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
	  (lambda()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Random file cleanup
(setq user-emacs-directory "~/.cache/emacs"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

(load-theme 'doom-dracula t)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;(use-package dired-single)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
    :map dired-mode-map
;;      (("C-b" . dired-single-up-directory)
;;      ("C-f" . dired-single-buffer)))
      (("C-b" . dired-up-directory)
      ("C-f" . dired-find-file)))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

;; Note: Need to make this hide/show work only in minor mode
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))
;  :bind
;  (")" . dired-hide-dotfiles-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 :map ivy-switch-buffer-map
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

(use-package all-the-icons)

(use-package doom-themes)
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom (doom-modeline-height 10))

;; Turn on indentation and auto-fill mode for Org files
(defun cust/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :bind (("C-c a" . custom-org-agenda)
	 ("C-c s" . org-agenda))
  :hook (org-mode . cust/org-mode-setup)
p  :config
  (defun custom-org-agenda ()
    (interactive)
    (org-agenda nil "c"))
  (setq org-ellipsis " ▾")
;	org-hide-emphasis-markers t
;	org-src-fontify-natively t
;	org-src-tab-acts-natively t
;	org-edit-src-content-intentation 0
;	org-hide-block-startup nil
;	org-src-preserve-indentation nil
;	org-startup-folded 'content
;	org-cycle-separator-lines 2)
(use-package org-bullets
;;  :if (not dw/is-termux)
  :after org
  :hook (org-mode . org-bullets-mode)))
  ;:custom
;  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))))

;; Make sure org-indent face is available
(require 'org-indent)

(setq org-agenda-files (directory-files-recursively "~/proj/orgfiles" "org"))


;; TODO keywords.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d)")
	(sequence "TASK(k)" "|" "DONE(d)")))

;; Show the daily agenda by default.
(setq org-agenda-span '3)

;; Hide tasks that are scheduled in the future
					;(setq org-agenda-todo-ignore-scheduled nil)

(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Hide the deadline prewarning prior to scheduled date.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

;; Customized view for the daily workflow.
(setq org-agenda-custom-commands
      '(("c" "Agenda / INTR / PROG / NEXT"
	 ((tags-todo "PRIORITY={A}"
		((org-agenda-overriding-header "High-priority unfinished tasks:")
		 (org-agenda-skip-function '(org-agenda-skip-entry-if 'done))))
     (agenda "")
     (todo "INTR")
      (todo "PROG")
      (todo "NEXT")))
      ("n" "Global Tasks"
	 ((agenda "" ((org-agenda-span 7)))
	  (alltodo "")))
      ("b" "Tasks Owned by Others"
       ((todo "TASK")))))

(use-package ox-reveal
  :config
  (setq org-reveal-root (concat "file://" (expand-file-name "~") "/proj/reveal.js"))
  (setq org-reveal-title-slide nil))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(setq org-src-tab-acts-natively t)

;; Automatically tangle our init.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/proj/dotfiles/"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package wc-mode
  :ensure t)

(use-package writegood-mode
  :ensure t)

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package ledger-mode
  :ensure t
  :mode ("\\.journal\\'" . ledger-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/proj")
    (setq projectile-project-search-path '("~/proj")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode)
  :after projectile)

(use-package term
  :config
  (setq explicit-shell-file-name "bash")
  ;;(setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;(setq explicit-shell-file-name "powershell.exe")
;(setq explicit-powershell.exe-args '())

(defun efs/configure-eshell()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop","zsh")))
  ;; Note: Type use-theme in eshell to see different themes
  (eshell-git-prompt-use-theme 'default))

(setq efs/exwm-enabled (and (eq window-system 'x)
                            (seq-contains-p command-line-args "--use-exwm")))

(when efs/exwm-enabled
  (load-file "~/.emacs.d/exwm.el"))

;; Use this with M-x global-command-log-mode and clm/toggle-command-log-mode
(use-package command-log-mode)
