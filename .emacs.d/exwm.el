(expand-file-name "~")

;; Runs a script in the background
(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

;; Open eshell by default
(eshell)

;; On status bar, add network status
(efs/run-in-background "nm-applet")
;; On status bar, add battery status
(efs/run-in-background "cbatticon"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
;  :init
;  (require 'exwm-systemtray)
;  (exwm-systemtray-enable) 
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When running a program in a buffer, set buffer name to program name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)
  
  ;; When EXWM loads up, load our init hook from exwm.org
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; These keys escape the EXWM process and get handed to emacs
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\M-&
	  ?\M-:
	  ?\C-\M-j
	  ?\C-\ ))

  ;; These keys bypass the escape keys above, allowing you to pass to EXWM instead
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Give me a system tray!
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; These are global EXWM keys to set
  (setq exwm-input-global-keys
	`(
	  ([?\s-r] . exwm-reset)
    ;; Navigate windows
	  ([s-left] . windmove-left)
	  ([s-right] . windmove-right)
	  ([s-up] . windmove-up)
	  ([s-down] . windmove-down)
    ;; Fullscreen programs. TODO: Apply to normal buffers too.
    ([?\s-f] . exwm-layout-toggle-fullscreen)
    ;; Allows interactive program list to start up
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
    ;; Prompts the user to select workspace
	  ([?\s-w] . exwm-workspace-switch)
    ;; Press s-# to get the workspace necessary
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9)))))

(exwm-enable)
