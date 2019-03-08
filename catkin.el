;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

(require 'helm)
(require 'xterm-color)

(defconst WS "EMACS_CATKIN_WS")

(defun catkin-set-ws (ws)
  (setenv WS ws)
  )

(defun catkin-cd (cmd)
  (let ((ws (getenv WS)))
    (if cmd (format "cd %s && %s" ws cmd))
    )
  )

(defun catkin-init ()
  "(Re-)Initializes a catkin workspace at path"
  (let ((ws (getenv WS)))
    (unless (file-exists-p ws)
      (unless (y-or-n-p (format "Path %s does not exist. Create?" ws))
        (error "Cannot initialize workspace `%s' since it doesn't exist" ws)
        )
      (make-directory (format "%s/src" ws) t)  ; also create parent directiories
      (call-process-shell-command (format "catkin init --workspace %s" ws))
      )
    )
  )

(defun catkin-source (command)
  "Prepends a source $EMACS_CATKINB_WS/devel/setup.bash before `command' if such a file exists."
  (let* ((ws (getenv WS))
         (setup-file (format "%s/devel/setup.bash" ws)))
    (if (file-exists-p setup-file)
        (format "source %s && %s" setup-file command)
      command
      )
    )
  )

(defun catkin-print-config ()
  "Prints the catkin config of $EMACS_CATKIN_WS to a new buffer called *catkin-config*"
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  (call-process-shell-command (format "catkin --force-color config --workspace %s" (getenv WS)) nil t)
  (xterm-color-colorize-buffer)
  (other-window 1)
  )

(defun catkin-build-finished (process signal)
  "This gets called, once the catkin build command finishes. It marks the buffer
   as read-only and asks to close the window"
  (when (memq (process-status process) '(exit signal))
    (message "Catkin build done!")
    (other-window 1)     ; select the first "other" window, i.e. the build window
    (evil-normal-state)  ; leave insert mode
    (read-only-mode)     ; mark as not-editable
    (when (y-or-n-p "Catkin build done. Close window?")
      (delete-window)
      )
    )
  )

(defun catkin-build-package (&optional pkgs)
  "Build the catkin workspace at $EMACS_CATKIN_WS after sourcing it's ws.
   If `pkgs' is non-nil, only these packages are built, otherwise all packages in the ws are build"
  (let* ((packages (mapconcat 'identity pkgs " "))
         (build-command (catkin-source (format "catkin build --workspace %s %s" (getenv WS) packages)))
         (buffer (get-buffer-create "*Catkin Build*"))
         (process (progn
                    (async-shell-command build-command buffer)
                    (get-buffer-process buffer)
                    ))
         )
    (if (process-live-p process)
        (set-process-sentinel process #'catkin-build-finished)
      (error "Could not attach process sentinel to \"catkin build\" since no such process is running")
      )
    )
  )

(defun catkin-list-of-command-output (command)
  (with-temp-buffer
    (call-process-shell-command command nil t)
    (split-string (buffer-string) "\n" t)
    )
  )

(defun catkin-list ()
  "Returns a list of all packages in the workspace at $EMACS_CATKIN_WS"
  (catkin-list-of-command-output
   (format "catkin list --workspace %s --unformatted --quiet" (getenv WS)))
  )

(defun catkin-build ()
  (interactive)
  (helm :sources '((name . "Catkin Build [package]")
                   (candidates . (lambda () (cons "[all]" (catkin-list))))
                   (action . (("Build" . (lambda (candidate)
                               (if (member "[all]" (helm-marked-candidates))
                                   (catkin-build-package)
                                 (catkin-build-package (helm-marked-candidates))
                               )))
                           )
                   ))
        )
  )


;; Tests
(catkin-set-ws "/tmp/hello/test/ws")
(catkin-init)
(catkin-print-config)
(catkin-source "catkin config")

;;; catkin.el ends here
