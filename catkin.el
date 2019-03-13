;;; catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  gollth
;; Keywords: tools, ROS
;; Package-Requires: (helm xterm-color el-mock)

;;; Commentary:

;; catkin is a package providing an interface to catkin-tools `https://catkin-tools.readthedocs.io/en/latest/'.
;; It integrates with `helm' such that the config is shown in a helm dialog and can be customized
;; with actions. The colored build output is put in a dedicated buffer.

;;; Code:

(require 'helm)
(require 'xterm-color)

(defconst catkin--WS "EMACS_CATKIN_WS")
(defun catkin--parse-config (key)
  (let ((path (format "%s/.catkin_tools/profiles/default/config.yaml" (getenv catkin--WS))))
    (if (null (file-exists-p path)) (error "Catkin workspace seems uninitialized. Use `(catkin-init)' to do that now"))

    (ignore-errors
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (re-search-forward (format "^%s: *\\(\\(\n- .*$\\)+\\|\\(.*$\\)\\)" key))
        (let ((match (match-string 1)))
          (with-temp-buffer
            (insert match)
            (cond ((string= (buffer-string) "[]") '())
                  ((= 1 (count-lines (point-min) (point-max))) (buffer-string))
                  (t (split-string (replace-regexp-in-string "^- \\|^\\W*$" "" (buffer-string)) "\n"))
                  )
            )
          )
        )
      )
    )
  )

(defun catkin--setup ()
  "Calls `catkin--set-ws' without arguments if $EMACS_CATKIN_WS is not set."
  (if (null (getenv catkin--WS)) (catkin--set-ws))
  )

(defun catkin--util-format-list (list sep)
  "Combines the elements of LIST into a string joined by SEP"
  (mapconcat 'identity list sep)
  )

(defun catkin--util-command-to-list (command &optional separator)
  "Returns each part of the stdout of COMMAND as elements of a list.
If SEPARATOR is nil, the newline character is used to split stdout."
  (let ((sep (if separator separator "\n")))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (split-string (substring (buffer-string) 0 -1) sep t)
      )
    )
  )
(defun catkin--util-absolute-path-of (pkg)
  "Returns the absolute path of PKG by calling \"rospack find ...\""
  (shell-command-to-string (catkin--source (format "printf $(rospack find %s)" pkg)))
  )

;;;###autoload
(defun catkin-set-workspace (&optional path)
  "Sets the current catkin workspace to PATH. If PATH is nil the user is prompted to enter the path"
  (interactive)
  (if path (catkin--set-ws path)
      (catkin--set-ws (helm-read-string "Set catkin workspace: " (getenv catkin--WS)))
      )
  (message (format "Catkin workspace set to %s" (getenv catkin--WS)))
  )

(defun catkin--set-ws (&optional ws)
  "Tells EMACS which workspace to use for all `catkin' commands.
It sets the environment variable EMACS_CATKIN_WS to the value of WS. When WS is nil
similar to `roscd' this function looks in all values within $CMAKE_PREFIX_PATH
and chooses the first one as WS which contains a '.catkin' file"
  (let ((cmake-prefix-path (getenv "CMAKE_PREFIX_PATH")))
    (cond (ws (setenv catkin--WS ws))
          ((null cmake-prefix-path)
           (error "Cannot automatically set catkin workspace because $CMAKE_PREFIX_PATH is not set.
Check the value of CMAKE_PREFIX_PATH with `setenv' and/or call `catkin--set-ws' with a path to your workspace (e.g. \"/opt/ros/kinetic\")"))
          (t (loop for path in (split-string cmake-prefix-path ":")
                 if (file-exists-p (format "%s/.catkin" path))
                 do (setenv catkin--WS path)
                 and do (message (format "Catkin: Setting workspace to %s" path))
                 and do (return)
                 finally do (error "Could not find any catkin workspace within $CMAKE_PREFIX_PATH")
                 )
             )
          )
    )
  )

;;;###autoload
(defun catkin-init ()
  "(Re-)Initializes a catkin workspace at $EMACS_CATKIN_WS"
  (interactive)
  (let ((ws (getenv catkin--WS)))
    (unless (file-exists-p ws)
      (unless (y-or-n-p (format "Path %s does not exist. Create?" ws))
        (error "Cannot initialize workspace `%s' since it doesn't exist" ws)
        )
      (make-directory (format "%s/src" ws) t)  ; also create parent directiories
      (call-process-shell-command (format "catkin init --workspace %s" ws))
      )
    )
  )

(defun catkin--source (command)
  "Prepends a `source $EMACS_CATKINB_WS/devel/setup.bash &&' before COMMAND if such a file exists."
  (let* ((ws (getenv catkin--WS))
         (setup-file (format "%s/devel/setup.bash" ws)))
    (if (file-exists-p setup-file)
        (format "source %s && %s" setup-file command)
      command
      )
    )
  )

;;;###autoload
(defun catkin-config-show ()
  "Prints the current configuration the catkin workspace at $EMACS_CATKIN_WS to a new buffer called *catkin-config*"
  (catkin--setup)
  (switch-to-buffer-other-window "*catkin-config*")
  (erase-buffer)
  ; Pipe stderr to null to supress "could not determine width" warning
  (call-process-shell-command (format "catkin --force-color config --workspace %s 2> /dev/null" (getenv catkin--WS)) nil t)
  (xterm-color-colorize-buffer)
  (other-window 1)
  )

(defun catkin-config-open ()
  "Opens the config file for the catkin profile for the workspace at $EMACS_CATKIN_WS"
  (find-file (format "%s/.catkin_tools/profiles/default/config.yaml" (getenv catkin--WS)))
  )

(defun catkin--config-args (operation &optional args)
  "Calls 'catkin config' for the workspace at $EMACS_CATKIN_WS to execute some OPERATION.
The ARGS are string joined with spaces and applied after the OPERATION. This function
can be used to set args of a certain type like so:

(catkin--config-args \"--cmake-args\" '(\"-DCMAKE_ARG1=foo\" \"-DCMAKE_ARG2=bar\"))
(catkin--config-args \"--no-make-args\")"
  (let ((arg-string (catkin--util-format-list args " ")))
    (substring
     (call-process-shell-command
      (format "catkin config --workspace %s %s %s" (getenv catkin--WS) operation arg-string)
      )
     0 -1)
   )
  )
(defun catkin--config-args-find (filter &optional sep)
  "Calls 'catkin config' and applies the FILTER to the output. After that
the resulting string is split at SEP into a list. If SEP is nil the
default split separator is space. This is useful to get a list of all
arguments of a certain type e.g. all cmake-args or all whitelist pkgs."
  (catkin--util-command-to-list
   ;; due to https://github.com/catkin/catkin_tools/issues/519 catkin config without args
   ;; clears make-args, thats why we use the -a switch to prevent that until this gets fixed
   ;; Supress stderr for "Could not determine width of terminal" warnings
   (format "catkin --no-color config -a --workspace %s 2> /dev/null | sed -n 's/%s//p'"
           (getenv catkin--WS)
           filter
           )
   (if sep sep " ")
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  CMAKE Args                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catkin-config-cmake-args ()
  "Returns a list of all currenty set cmake args for the workspace at $EMACS_CATKIN_WS"
  (catkin--parse-config "cmake_args")
  )
(defalias 'catkin-config-cmake-args-clear (apply-partially 'catkin--config-args "--no-cmake-args")
  "Removes all cmake args for the current workspace at $EMACS_CATKIN_WS"
  )
(defalias 'catkin-config-cmake-args-set (apply-partially 'catkin--config-args "--cmake-args")
  "Sets a list of cmake args for the current workspace at $EMACS_CATKIN_WS. Passing an empty list to ARGS will clear all currently set args."
  )

(defalias 'catkin-config-cmake-args-add (apply-partially 'catkin--config-args "--append-args --cmake-args")
  "Adds a list of cmake args to the existing set of cmake args for the current workspace at $EMACS_CATKIN_WS."
  )
(defalias 'catkin-config-cmake-args-remove (apply-partially 'catkin--config-args "--remove-args --cmake-args")
  "Removes a list of cmake args from the existing set of cmake args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and are just ignored."
  )
(defun catkin-config-cmake-change (arg)
  "Prompts the user to enter a new value for a CMake arg. The prompt in the
minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for CMake Arg: " arg)))
    (catkin-config-cmake-args-remove (list arg))
    (catkin-config-cmake-args-add (list new-arg))
    )
  )
(defun catkin-config-cmake-new (&optional _)
  "Prompts the user to enter a new CMake arg which will be returned."
  (interactive)
  (catkin-config-cmake-args-add (list (helm-read-string "New CMake Arg: ")))
  )
(defvar catkin--config-cmake-sources
  (helm-build-sync-source "CMake"
    :candidates 'catkin-config-cmake-args
    :action '(
              ("Change" . (lambda (x) (catkin-config-cmake-change x) (catkin)))
              ("Add" . (lambda (x) (catkin-config-cmake-new x) (catkin)))
              ("Clear" . (lambda (_) (catkin-config-cmake-args-remove (helm-marked-candidates)) (catkin)))
              )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   MAKE Args                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catkin-config-make-args ()
  "Returns a list of all currenty set make args for the workspace at $EMACS_CATKIN_WS"
  (catkin--parse-config "make_args")
  )
(defalias 'catkin-config-make-args-clear (apply-partially 'catkin--config-args "--no-make-args")
  "Removes all make args for the current workspace at $EMACS_CATKIN_WS"
  )
(defalias 'catkin-config-make-args-set (apply-partially 'catkin--config-args "--make-args")
  "Sets a list of make args for the current workspace at $EMACS_CATKIN_WS.
Passing an empty list to ARGS will clear all currently set args."
  )
(defalias 'catkin-config-make-args-add (apply-partially 'catkin--config-args "--append-args --make-args")
  "Adds a list of make args to the existing set of make args for the current workspace at $EMACS_CATKIN_WS."
  )
(defalias 'catkin-config-make-args-remove (apply-partially 'catkin--config-args "--remove-args --make-args")
  "Removes a list of make args from the existing set of make args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and
are just ignored."
  )
(defun catkin-config-make-change (arg)
  "Prompts the user to enter a new value for a Make arg. The prompt in the
minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for Make Arg: " arg)))
    (catkin-config-make-args-remove (list arg))
    (catkin-config-make-args-add (list new-arg))
    )
  )
(defun catkin-config-make-new (&optional _)
  "Prompts the user to enter a new Make arg which will be returned."
  (interactive)
  (catkin-config-make-args-add (list (helm-read-string "New Make Arg: ")))
  )
(defvar catkin--config-make-sources
  (helm-build-sync-source "Make"
    :candidates 'catkin-config-make-args
    :action '(
              ("Change" . (lambda (x) (catkin-config-make-change x) (catkin)))
              ("Add"    . (lambda (x) (catkin-config-make-new x) (catkin)))
              ("Clear"  . (lambda (_) (catkin-config-make-args-remove (helm-marked-candidates)) (catkin)))
              )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   CATKIN-MAKE Args                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catkin-config-catkin-make-args ()
  "Returns a list of all currenty set catkin-make args for the workspace at $EMACS_CATKIN_WS"
  (catkin--parse-config "catkin_make_args")
  )
(defalias 'catkin-config-catkin-make-args-clear (apply-partially 'catkin--config-args "--no-catkin-make-args")
  "Removes all catkin-make args for the current workspace at $EMACS_CATKIN_WS"
  )
(defalias 'catkin-config-catkin-make-args-set (apply-partially 'catkin--config-args "--catkin-make-args")
  "Sets a list of catkin-make args for the current workspace at $EMACS_CATKIN_WS.
Passing an empty list to ARGS will clear all currently set args."
  )

(defalias 'catkin-config-catkin-make-args-add (apply-partially 'catkin--config-args "--append-args --catkin-make-args")
  "Adds a list of catkin-make args to the existing set of catkin-make args for the current workspace at $EMACS_CATKIN_WS."
  )
(defalias 'catkin-config-catkin-make-args-remove (apply-partially 'catkin--config-args "--remove-args --catkin-make-args")
  "Removes a list of catkin-make args from the existing set of catkin-make args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and
are just ignored."
  )
(defun catkin-config-catkin-make-change (arg)
  "Prompts the user to enter a new value for a Catkin-Make arg. The prompt in the
minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for Catkin-Make Arg: " arg)))
    (catkin-config-catkin-make-args-remove (list arg))
    (catkin-config-catkin-make-args-add (list new-arg))
    )
  )

(defun catkin-config-catkin-make-new (&optional _)
  "Prompts the user to enter a new Catkin-Make arg which will be returned."
  (interactive)
  (catkin-config-catkin-make-args-add (list (helm-read-string "New Catkin-Make Arg: ")))
  )

(defvar catkin--config-catkin-make-sources
  (helm-build-sync-source "Catkin-Make"
    :candidates 'catkin-config-catkin-make-args
    :action '(
              ("Change" . (lambda (x) (catkin-config-catkin-make-change x) (catkin)))
              ("Add" . (lambda (x) (catkin-config-catkin-make-new x) (catkin)))
              ("Clear" . (lambda (_) (catkin-config-catkin-make-args-remove (helm-marked-candidates)) (catkin)))
              )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Whitelist/Blacklist                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catkin-config-whitelist ()
  "Returns a list of all currenty whitelisted packages for the workspace at $EMACS_CATKIN_WS"
  (catkin--parse-config "whitelist")
  )
(defalias 'catkin-config-whitelist-add (apply-partially 'catkin--config-args "--append-args --whitelist")
  "Marks a list of packages to be whitelisted for the current workspace at $EMACS_CATKIN_WS."
  )
(defalias 'catkin-config-whitelist-remove (apply-partially 'catkin--config-args "--remove-args --whitelist")
  "Removes a list of whitelisted packages from the existing whitelist for
the current workspace at $EMACS_CATKIN_WS. Packages which are currently
not whitelisted and are requested to be removed don't provoce an error and
are just ignored."
  )
(defvar catkin--config-whitelist-sources
  (helm-build-sync-source "Whitelist"
    :candidates 'catkin-config-whitelist
    :action '(("Un-Whitelist" . (lambda (_) (catkin-config-whitelist-remove (helm-marked-candidates)) (catkin))))
    )
  )

(defun catkin-config-blacklist ()
  "Returns a list of all currenty blacklisted packages for the workspace at $EMACS_CATKIN_WS"
  (catkin--parse-config "blacklist")
  )
(defalias 'catkin-config-blacklist-add (apply-partially 'catkin--config-args "--append-args --blacklist")
  "Marks a list of packages to be blacklisted for the current workspace at $EMACS_CATKIN_WS."
  )
(defalias 'catkin-config-blacklist-remove (apply-partially 'catkin--config-args "--remove-args --blacklist")
  "Removes a list of blacklisted packages from the existing blacklist for
the current workspace at $EMACS_CATKIN_WS. Packages which are currently
not blacklisted and are requested to be removed don't provoce an error and are just ignored."
  )
(defvar catkin--config-blacklist-sources
  (helm-build-sync-source "Blacklist"
    :candidates 'catkin-config-blacklist
    :action '(("Un-Blacklist" . (lambda (_) (catkin-config-blacklist-remove (helm-marked-candidates)) (catkin))))
    )
  )
(defvar catkin--config-packages-sources
  (helm-build-sync-source "Packages"
    :candidates 'catkin-list
    :action '(("Build" . (lambda (c) (catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates))))
              ("Blacklist" . (lambda (_) (catkin-config-blacklist-add (helm-marked-candidates)) (catkin)))
              ("Whitelist" . (lambda (_) (catkin-config-whitelist-add (helm-marked-candidates)) (catkin)))
              )
    )
  )

;;;###autoload
(defun catkin ()
  "Opens a helm query which shows the current config for the catkin workspace at $EMACS_CATKIN_WS.
It combines the different arguments into helm sections:

 - CMake Arguments          actions: change, add, Clear
 - Make Arguments           actions: change, add, Clear
 - Catkin Make Arguments    actions: change, add, Clear
 - Whitelisted Packages     actions: Un-Whitelist
 - Blacklisted Packages     actions: Un-Blacklist
 - All Packages             actions: Build, open folder, Open CMakeLists.txt, Open package.xml, Blacklist, Whitelist

Each section has a distinct set of actions for each item. Actions above starting with a capital
letter accept multiple arguments from that section (which you can mark in helm via C-SPC). For
example you can mark several CMake arguments and then choose the 'Clear' action (F3), which will
clear all the marked arguments from the current catkin config. Actions not starting with captial
letters will only affect the item in the list which is currently selected regardless of which
other items are marked.

Alternatively you can mark all items in the current section with M-a, e.g. to build your entire
workspace go to the package section and press M-a to mark all packages and press enter to choose
the default action (F1) 'Build'.

After you have executed an action the helm dialog will show again (execpt for Build and Open actions).
To quit it just press ESC.
   "
  (interactive)
  (catkin--setup)
  (helm :buffer "* Catkin *"
        :sources '(catkin--config-cmake-sources
                   catkin--config-make-sources
                   catkin--config-catkin-make-sources
                   catkin--config-whitelist-sources
                   catkin--config-blacklist-sources
                   catkin--config-packages-sources
                   )
        )
  )

(defun catkin--build-finished (process signal)
  "This gets called, once the catkin build command finishes. It marks the buffer as read-only and asks to close the window"
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
  "Build the catkin workspace at $EMACS_CATKIN_WS after sourcing it's ws. If PKGS is non-nil, only these packages are built, otherwise all packages in the ws are build"
  (let* ((packages (catkin--util-format-list pkgs " "))
         (build-command (format "catkin build --workspace %s %s" (getenv catkin--WS) packages))
         (buffer (get-buffer-create "* Catkin Build *"))
         (process (progn
                    (async-shell-command build-command buffer)
                    (get-buffer-process buffer)
                    ))
         )
    (if (process-live-p process)
        (set-process-sentinel process #'catkin--build-finished)
      (error "Could not attach process sentinel to \"catkin build\" since no such process is running")
      )
    )
  )

(defun catkin-list ()
  "Returns a list of all packages in the workspace at $EMACS_CATKIN_WS"
  (catkin--util-command-to-list
   (format "catkin list --workspace %s --unformatted --quiet" (getenv catkin--WS)))
  )

(defun catkin-open-file-in (pkg file)
  "Opens the file at `$(rospack find pkg)/fil'. FILE can be a relative path to PKG."
  (interactive)
  (catkin--setup)
  (find-file (format "%s/%s" (catkin--util-absolute-path-of pkg) file))
  )

(defun catkin-open-pkg-cmakelist (pkgs)
  "Opens the `CMakeLists.txt' file for each of the package names within PKGS"
  (loop for pkg in pkgs
        do (catkin-open-file-in pkg "CMakeLists.txt")
        )
  )

(defun catkin-open-pkg-package (pkgs)
  "Opens the `package.xml' file for each of the package names within PKGS"
  (loop for pkg in pkgs
        do (catkin-open-file-in pkg "package.xml")
        )
  )

(defun catkin-open-pkg-dired (pkg)
  "Opens the absolute path of PKG in `dired'."
  (interactive)
  (catkin--setup)
  (dired (catkin--util-absolute-path-of pkg))
  )

;;;###autoload
(defun catkin-build ()
  "Prompts the user via a helm dialog to select one or more
packages to build in the current workspace. `C-SPC' will enable
multiple selections while `M-a' selects all packages."
  (interactive)
  (catkin--setup)
  (helm :buffer "* Catkin Build *"
        :sources (helm-build-sync-source "Packages"
                   :candidates (catkin-list)
                   :fuzzy-match t
                   :action '(("Build" . (lambda (c) (catkin-build-package (helm-marked-candidates))))
                             ("Open Folder" . catkin-open-pkg-dired)
                             ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
                             ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates))))
                             )
                   )
        )
  )

(define-minor-mode global-catkin-mode
  "A minor mode that enable the keybindings for catkin"
  :init-value t
  :lighter "Catkin"
  :keymap (let ((keymap (make-sparse-keymap)))
            (define-key keymap (kbd "C-x c c") 'catkin)
            (define-key keymap (kbd "C-x c b") 'catkin-build)
            (define-key keymap (kbd "C-x c l") 'catkin-config-show)
            (define-key keymap (kbd "C-x c w") 'catkin-set-workspace)
            keymap)
  :global t
  )


(provide 'catkin)

;;; catkin.el ends here
