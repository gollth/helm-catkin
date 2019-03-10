;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS

;;; Code:

(require 'helm)
(require 'xterm-color)

(defconst WS "EMACS_CATKIN_WS")

(defun catkin-util-format-list (list sep)
  (mapconcat 'identity list sep)
  )

(defun catkin-util-command-to-list (command &optional separator)
  "Returns each part of the stdout of `command' as elements of a list.
   If `separator' is nil, the newline character is used to split stdout."
  (let ((sep (if separator separator "\n")))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (split-string (buffer-string) sep t)
      )
    )
  )


(defun catkin-set-ws (&optional ws)
  (if ws
      (setenv WS ws)
    (loop for path in (split-string (getenv "CMAKE_PREFIX_PATH") ":")
          if (file-exists-p (format "%s/.catkin" path))
          do (setenv WS path)
          and do (message (format "Catkin: Setting workspace to %s" path))
          and do (return)
          finally do (error "Could not find any catkin workspace within $CMAKE_PREFIX_PATH =(")
          )
    )
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

(defun catkin-config-print ()
  "Prints the catkin config of $EMACS_CATKIN_WS to a new buffer called *catkin-config*"
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  ; Pipe stderr to null to supress "could not determine width" warning
  (call-process-shell-command (format "catkin --force-color config --workspace %s 2> /dev/null" (getenv WS)) nil t)
  (xterm-color-colorize-buffer)
  (other-window 1)
  )

(defun catkin-config-cmake-args ()
  "Returns a list of all currenty set cmake args for the workspace
   at $EMACS_CATKIN_WS"
  (catkin-util-command-to-list
   ; Supress stderr for "Could not determine width of terminal" warnings
   (format "catkin --no-color config --workspace %s 2> /dev/null | sed -n 's/Additional CMake Args:\s*//p'"
           (getenv WS)
           )
   " "
   )
  )

(defun catkin-config-no-cmake-args ()
  "Removes all cmake args for the curArent workspace at $EMACS_CATKIN_WS"
(message "Clearing all cmake args...")
  (call-process-shell-command
   (format "catkin config --workspace %s --no-cmake-args" (getenv WS))
   )
  )

(defun catkin-config-set-cmake-args (args)
  "Sets a list of cmake args for the current workspace at $EMACS_CATKIN_WS.
   Passing an empty list to `args' will clear all currently set args."
  (if (null args) (catkin-config-no-cmake-args)
    (message "setting args to %s" args)
    (call-process-shell-command
     (format "catkin config --workspace %s --cmake-args %s"
             (getenv WS)
             (catkin-util-format-list args " ")
             )
     )
    )
  )

(defun catkin-config-add-cmake-args (args)
  "Adds a list of cmake args to the existing set of cmake args for the
   current workspace at $EMACS_CATKIN_WS."
  (call-process-shell-command
   (format "catkin config --workspace %s --append-args --cmake-args %s"
            (getenv WS)
            (catkin-util-format-list args " ")
            )
   )
  )

(defun catkin-config-remove-cmake-args (args)
  "Removes a list of cmake args from the existing set of cmake args for
   the current workspace at $EMACS_CATKIN_WS. Args which are currently
   not set and are requested to be removed don't provoce an error and
   are just ignored."
  (call-process-shell-command
   (format "catkin config --workspace %s --remove-args --cmake-args %s"
           (getenv WS)
           (catkin-util-format-list args " ")
           )
   )
  )

(defvar catkin-config-cmake-sources
  (helm-build-sync-source "CMake"
    :candidates 'catkin-config-cmake-args
    :action '(("Clear" . (lambda (_) (catkin-config-remove-cmake-args (helm-marked-candidates)))))
    )
  )

(defvar catkin-config-make-sources
  (helm-build-sync-source "Make"
    :candidates '("-j4" "-h")
    )
  )

(defun catkin-config ()
  (interactive)
  (helm :buffer "*helm catkin config*"
        :sources '(catkin-config-cmake-sources catkin-config-make-sources)
        )
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
  (let* ((packages (catkin-util-format-list pkgs " "))
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

(defun catkin-list ()
  "Returns a list of all packages in the workspace at $EMACS_CATKIN_WS"
  (catkin-util-command-to-list
   (format "catkin list --workspace %s --unformatted --quiet" (getenv WS)))
  )

(defun catkin-list-candidates (&optional include-all-option)
  "Assembes the list of packages in the current workspace.
   If the `include-all-option' parameter is non-nil another
   item with the value \"[*]\" is prepended to the list."
  (if include-all-option
      (cons "[*]" (catkin-list))
    ; else
    (catkin-list)
    )
  )

(defun catkin-get-absolute-path-of-pkg (pkg)
  "Returns the absolute path of `pkg' by calling \"rospack find ...\""
  (shell-command-to-string (catkin-source (format "printf $(rospack find %s)" pkg)))
  )

(defun catkin-open-file-in (pkg file)
  "Opens the file at \"$(rospack find pkg)/file\". `file' can be a
   relative path to `pkg'."
  (interactive)
  (find-file (format "%s/%s" (catkin-get-absolute-path-of-pkg pkg) file))
  )

(defun catkin-open-pkg-cmakelist (pkgs)
  "Opens the 'CMakeLists.txt' file for each of the package names within `pkgs'"
  (loop for pkg in pkgs
        do (catkin-open-file-in pkg "CMakeLists.txt")
        )
  )

(defun catkin-open-pkg-package (pkgs)
  "Opens the 'package.xml' file for each of the package names within `pkgs'"
  (loop for pkg in pkgs
        do (catkin-open-file-in pkg "package.xml")
        )
  )

(defun catkin-open-pkg-dired (pkg)
  "Opens the absolute path of `pkg' in dired."
  (interactive)
  (dired (catkin-get-absolute-path-of-pkg pkg))
  )

(defun catkin-build ()
  "Prompts the user via a helm dialog to select one or more
   packages to build in the current workspace. C-SPC will enable
   multiple selections while M-a selects all packages."
  (interactive)
  (helm :buffer "*helm catkin list*"
        :sources (helm-build-sync-source "Packages"
                   :candidates (catkin-list-candidates)
                   :fuzzy-match t
                   :action '(("Build" . (lambda (c) (catkin-build-package (helm-marked-candidates))))
                             ("Open Folder" . catkin-open-pkg-dired)
                             ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
                             ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates))))
                             )
                   )
        )
  )


;; Tests
(catkin-set-ws "/tmp/hello/test/ws")
(catkin-init)
(catkin-print-config)
(catkin-source "catkin config")

;;; catkin.el ends here
