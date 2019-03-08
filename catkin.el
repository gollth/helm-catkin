;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

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
  (when (memq (process-status process) '(exit signal))
    (message "Catkin build done!")
    (switch-to-buffer-other-window "*Catkin Build*")
    (evil-normal-state)   ; leave insert mode
    (other-window 1)
    )
  )
(defun catkin-build ()
  "Build the catkin workspace at $EMACS_CATKIN_WS"
  (let* ((buffer (get-buffer-create "*Catkin Build*"))
         (process (progn
                    (async-shell-command (format "catkin build --workspace %s" (getenv WS)) buffer)
                    (get-buffer-process buffer)
                    )))
    (if (process-live-p process)
        (set-process-sentinel process #'catkin-build-finished)
      (message "No process running")
      )
    )
  )

;; Tests
(catkin-set-ws "/tmp/hello/test/ws")
(catkin-init)
(catkin-print-config)
(catkin-source "catkin config")

;;; catkin.el ends here
