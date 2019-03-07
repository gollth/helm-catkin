;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

(defun catkin-cd (ws cmd)
  (format "cd %s && %s" ws cmd))

(defun catkin-init (ws)
  "(Re-)Initializes a catkin workspace at path `ws'"
  (unless (file-exists-p ws)
    (unless (y-or-n-p (format "Path %s does not exist. Create?" ws))
      (error "Cannot initialize workspace `%s' since it doesn't exist" ws))
    (make-directory ws)
    (make-directory (format "%s/src" ws)))
  (call-process-shell-command (catkin-cd ws "catkin init")))

(defun catkin-source (ws command)
  "Prepends a source `ws'/devel/setup.bash before `command'"
  (let ((setup-file (format "%s/devel/setup.bash" ws)))
    (if (file-exists-p setup-file)
        (format "source %s && %s" setup-file command)
      command)
     ))

(defun catkin-print-config (ws)
  "Prints the catkin config of `ws' to a new buffer called *catkin-config*"
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  (call-process-shell-command (catkin-source ws (catkin-cd ws "catkin config")) nil t)
  (other-window 1))

(catkin-print-config "~/hello")

;;; catkin.el ends here
