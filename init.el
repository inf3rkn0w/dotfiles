
;; PacMan stuff
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

;; Use this with M-x global-command-log-mode and clm/toggle-command-log-mode
(use-package command-log-mode)

(setq inhibit-startup-message t)
(setq inhibit-scratch-message nil) ;; Change with initial-scratch-message

;; Performance
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

;;(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts

;;(use-package all-the-icons)

;; exwm
;;(require 'exwm)
;;(require 'exwm-config)
;;(exwm-config-default)

;; Typefacing/Display
(set-face-attribute 'default nil :font "Fira Code Retina" :height 100)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package doom-themes)
  (use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)
    :custom (doom-modeline-height 10))
(load-theme 'doom-acario-dark t)

;; Ivy
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
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Turn on indentation and auto-fill mode for Org files
(defun cust/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . cust/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-edit-src-content-intentation 0
	org-hide-block-startup nil
	org-src-preserve-indentation nil
	org-startup-folded 'content
	org-cycle-separator-lines 2)
(use-package org-bullets
;;  :if (not dw/is-termux)
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(set-face-attribute 'org-level-1 nil :height 1.4)
(set-face-attribute 'org-level-2 nil :height 1.2)
(set-face-attribute 'org-level-3 nil :font "Cantarell" :weight 'regular :height 1.1)
(set-face-attribute 'org-level-4 nil :font "Cantarell" :weight 'regular :height 1.05)
;;(set-face-attribute 'org-level-5 nil :font "Cantarell" :weight 'regular :height 1)
;;(set-face-attribute 'org-level-6 nil :font "Cantarell" :weight 'regular :height 1)
;;(set-face-attribute 'org-level-7 nil :font "Cantarell" :weight 'regular :height 1)
;;(set-face-attribute 'org-level-8 nil :font "Cantarell" :weight 'regular :height 1)

;;(use-package general
;;  :config
;;  (general-create-definer cust/leader-keys
;;			  :keymaps '(emacs)
;;			  :global-prefix "C-x v")
;;  (cust/leader-keys
;;   "t" '(counsel-load-theme :which-key "choose theme")))
			  
(use-package magit)



(defun cust/set-markdown-header-font-sizes()
  (dolist (face '((markdown-header-face-1 . 1.4)
		  (markdown-header-face-2 . 1.2)
		  (markdown-header-face-3 . 1.1)
		  (markdown-header-face-4 . 1.0)
		  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

;; Use visual-line-mode in markdown-mode
(defun cust/markdown-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode)
  (cust/set-markdown-header-font-sizes))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :hook (markdown-mode . cust/markdown-mode-hook))

(use-package wc-mode
  :ensure t)

(use-package writegood-mode
  :ensure t)
  
(use-package ledger-mode
  :ensure t
  :mode ("\\.journal\\'" . ledger-mode))

;; Projectile
;; This was V1
;; (use-package projectile
;;   :bind
;;   (:map projectile-mode-map
;;   ("C-c p" . projectile-command-map))
;;   :config
;;   (projectile-mode +1))

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




;; Look up Hydra for font scaling


;; ;; Dired Project Explorer
;; (use-package dired-subtree
;;   :demand
;;   :bind
;;   (:map dired-mode-map
;;     ("<enter>" . cust/dwim-toggle-or-open)
;;     ("<return>" . cust/dwim-toggle-or-open)
;;     ("<tab>" . cust/dwim-toggle-or-open)
;;     ("<down-mouse-1>" . cust/mouse-dwim-to-toggle-or-open))
;;   :config
;;   (progn
;;     ;; Function to customize the line prefixes (I simply indent the lines a bit)
;;     (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
;;     (setq dired-subtree-use-backgrounds nil)))

;; (defun cust/dwim-toggle-or-open ()
;;   "Toggle subtree or open the file."
;;   (interactive)
;;   (if (file-directory-p (dired-get-file-for-visit))
;;       (progn
;;     (dired-subtree-toggle)
;;     (revert-buffer))
;;     (dired-find-file)))

;; (defun cust/mouse-dwim-to-toggle-or-open (event)
;;   "Toggle subtree or the open file on mouse-click in dired."
;;   (interactive "e")
;;   (let* ((window (posn-window (event-end event)))
;;      (buffer (window-buffer window))
;;      (pos (posn-point (event-end event))))
;;     (progn
;;       (with-current-buffer buffer
;;     (goto-char pos)
;;     (cust/dwim-toggle-or-open)))))

;; (use-package dired
;;   :ensure nil
;;   :config
;;   (progn
;;     (setq insert-directory-program "/usr/bin/ls")
;;     (setq dired-listing-switches "-lXGh --group-directories-first")
;;     (add-hook 'dired-mode-hook 'dired-omit-mode)
;;     (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

;; (defun cust/toggle-project-explorer ()
;;   "Toggle the project explorer window."
;;   (interactive)
;;   (let* ((buffer (dired-noselect (projectile-project-root)))
;;     (window (get-buffer-window buffer)))
;;     (if window
;;     (cust/hide-project-explorer)
;;       (cust/show-project-explorer))))

;; (defun cust/show-project-explorer ()
;;   "Project dired buffer on the side of the frame.
;; Shows the projectile root folder using dired on the left side of
;; the frame and makes it a dedicated window for that buffer."
;;   (let ((buffer (dired-noselect (projectile-project-root))))
;;     (progn
;;       (display-buffer-in-side-window buffer '((side . left) (window-width . 0.2)))
;;       (set-window-dedicated-p (get-buffer-window buffer) t))))

;; (defun cust/hide-project-explorer ()
;;   "Hide the project-explorer window."
;;   (let ((buffer (dired-noselect (projectile-project-root))))
;;     (progn
;;       (delete-window (get-buffer-window buffer))
;;       (kill-buffer buffer))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (muse writegood-mode wc-mode counsel-projectile projectile ledger-mode markdown-mode magit org-bullets which-key use-package rainbow-delimiters ivy-rich helpful doom-themes doom-modeline counsel command-log-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

