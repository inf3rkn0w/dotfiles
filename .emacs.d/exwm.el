(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(use-package exwm
;  :init
;  (require 'exwm-systemtray)
;  (exwm-systemtray-enable) 
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)
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
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
  (setq exwm-input-global-keys
	`(
	  ([?\s-r] . exwm-reset)
	  ([s-left] . windmove-left)
	  ([s-right] . windmove-right)
	  ([s-up] . windmove-up)
	  ([s-down] . windmove-down)
        ([s-f] . exwm-layout-toggle-fullscreen)

	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nill command)))

	  ([?\s-w] . exwm-workspace-switch)

	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))
  
  (exwm-enable))
