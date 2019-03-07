;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

(defun catkin-source-command (ws command)
  "Prepends a source `ws'/devel/setup.bash before `command', if such a file exists"
  (let ((setup-file (format "%s/devel/setup.bash" ws)))
    (if (file-exists-p setup-file)
        (format "source %s && %s" setup-file command)
      command
      )))

(defun catkin-print-config (ws)
  "Prints the catkin config of `ws' to a new buffer called *catkin-config*"
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  (shell-cd ws)
  (catkin-source-setup ws)
  (call-process-shell-command (catkin-source-command ws "catkin config") nil t)
  (other-window))

(catkin-print-config "~/ros/util")

;;; catkin.el ends here
