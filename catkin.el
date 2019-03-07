;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

(defun catkin-source-command (ws command)
  (format "source %s/devel/setup.bash && %s" ws command))

(defun catkin-print-config (ws)
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  (shell-cd ws)
  (catkin-source-setup ws)
  (call-process-shell-command (catkin-source-command ws "catkin config") nil t)
  (other-window))

(catkin-print-config "~/ros/util")

;;; catkin.el ends here
