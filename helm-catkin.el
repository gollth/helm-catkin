;;; helm-catkin.el --- Package for compile ROS workspaces with catkin-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thore Goll

;; Author:  Thore Goll <thoregoll@googlemail.com>
;; Keywords: catkin, helm, build, tools, ROS
;; Package-Requires: ((emacs "24.3") helm xterm-color (cl-lib "0.5"))
;; Homepage: https://github.com/gollth/helm-catkin
;; Version: 1.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; helm-catkin is a package providing an interface to catkin-tools `https://catkin-tools.readthedocs.io/en/latest/'.
;; It integrates with `helm' such that the config is shown in a helm dialog and can be customized
;; with actions.

;; Besides adjusting the config, you can build the ROS packages in the workspace in a colored build buffer.

;; All `helm-catkin' functions require a workspace defined. This is saved in a local environment
;; variable within EMACS called `EMACS_CATKIN_WS'. It is tried that the workspace is guessed
;; based on your current $CMAKE_PREFIX_PATH (similar like `roscd' does this). When this is
;; not working the catkin command will report an error. You should then explicately call
;; `helm-catkin-set-workspace' which asks you to enter a path to your workspace. This command
;; can also be used to change between different workspaces

;; Quick overview of provided functionality:
;; `helm-catkin-set-workspace'  :: Sets the path to the helm-catkin workspace for all further helm-catkin commands
;; `helm-catkin-workspace'      :: Returns and reports the value of the currently set workspace
;; `helm-catkin'                :: Main command for showing, configuring and building in a helm window
;; `helm-catkin-build'          :: Build one, multiple or all packages in the current workspace
;; `helm-catkin-init'           :: Initializes the workspace and create a src/ folder if it doesn't exist
;; `helm-catkin-clean'          :: Clean the workspace (remove build/, devel/ and install/ folders)
;; `helm-catkin-config-show'    :: Shows the current config in a new buffer
;; `helm-catkin-config-open'    :: Opens the .catkin_tools/profiles/default/config.yaml file in a buffer

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'xterm-color)

(define-derived-mode helm-catkin-mode special-mode "Catkin")

(defconst helm-catkin--WS "EMACS_CATKIN_WS")
(defun helm-catkin--parse-config (key)
  (let* ((ws (getenv helm-catkin--WS))
         (path (format "%s/.catkin_tools/profiles/default/config.yaml" ws)))
    (if (null (file-exists-p path)) (error "Catkin workspace '%s'  seems uninitialized. Use `(helm-catkin-init)' to do that now" ws))

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
                  (t (split-string (replace-regexp-in-string "^- \\|^\\W*$" "" (buffer-string)) "\n")))))))))

(defun helm-catkin--setup ()
  "Call `helm-catkin--set-ws' without arguments if $EMACS_CATKIN_WS is not set."
  (if (null (getenv helm-catkin--WS)) (helm-catkin--set-ws)))

(defun helm-catkin--util-format-list (list sep)
  "Combines the elements of LIST into a string joined by SEP."
  (mapconcat 'identity list sep))

(defun helm-catkin--util-command-to-list (command &optional separator)
  "Return each part of the stdout of COMMAND as elements of a list.
If SEPARATOR is nil, the newline character is used to split stdout."
  (let ((sep (if separator separator "\n")))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (ignore-errors (split-string (substring (buffer-string) 0 -1) sep t)))))

(defun helm-catkin--util-absolute-path-of (pkg)
  "Return the absolute path of PKG by calling \"rospack find ...\".
If the package cannot be found this command raises an error."
  (with-temp-buffer
    (delete-file "/tmp/.catkin-error")
    (call-process-shell-command (helm-catkin--source (format "printf $(rospack find %s)" pkg))
                                nil
                                '(t "/tmp/.catkin-error"))
    (with-temp-buffer
      (insert-file-contents "/tmp/.catkin-error")
      (unless (string= (buffer-string) "") (error (buffer-string))))
    (buffer-string)))

;;;###autoload
(defun helm-catkin-workspace ()
  "Print the currently set helm-catkin workspace to the message line.
The value as a string is returned."
  (interactive)
  (helm-catkin--setup)
  (let ((ws (getenv helm-catkin--WS)))
    (message (format "Catkin workspace currently set to: '%s'" ws))
    ws))

;;;###autoload
(defun helm-catkin-set-workspace (&optional path)
  "Set the current catkin workspace to PATH. If PATH is nil the user is prompted to enter the path."
  (interactive)
  (if path (helm-catkin--set-ws path)
      (helm-catkin--set-ws (read-directory-name "Set catkin workspace: " (getenv helm-catkin--WS))))
  (message (format "Catkin workspace set to %s" (getenv helm-catkin--WS))))

(defun helm-catkin--set-ws (&optional ws)
  "Tell EMACS which workspace to use for all `helm-catkin' commands.
It sets the environment variable EMACS_CATKIN_WS to the value of WS. When WS is
nilA similar to `roscd' this function looks in all values within
$CMAKE_PREFIX_PATH and chooses the first one as WS which contains a '.catkin' file"
  (let ((cmake-prefix-path (getenv "CMAKE_PREFIX_PATH")))
    (cond (ws (setenv helm-catkin--WS (expand-file-name ws)))
          ((null cmake-prefix-path)
           (error "Cannot automatically set catkin workspace because $CMAKE_PREFIX_PATH is not set.
Check the value of CMAKE_PREFIX_PATH with `setenv' and/or call `helm-catkin-set-workspace' with a path to your workspace (e.g. \"/opt/ros/kinetic\")"))
          (t (loop for path in (split-string cmake-prefix-path ":")
                 if (file-exists-p (format "%s/.catkin" path))
                 do (setenv helm-catkin--WS (expand-file-name (format "%s/.." path)))
                 and do (message (format "Catkin: Setting workspace to %s" (expand-file-name path)))
                 and do (return)
                 finally do (error "Could not find any catkin workspace within $CMAKE_PREFIX_PATH"))))))

;;;###autoload
(defun helm-catkin-init ()
  "(Re-)Initialize a catkin workspace at $EMACS_CATKIN_WS.
Creates the folder if it does not exist and also a child 'src' folder."
  (interactive)
  ;; If current workspace is null, prompt the user for it
  (if (null (getenv helm-catkin--WS)) (helm-catkin-set-workspace))
  (let ((ws (getenv helm-catkin--WS)))
    (unless (file-exists-p ws)
      (unless (y-or-n-p (format "Path %s does not exist. Create? " ws))
        (error "Cannot initialize workspace `%s' since it doesn't exist" ws))
      (make-directory (format "%s/src" ws) t)  ; also create parent directiories)
    ;; Now that everything should be setup, call catkin config --init
    ;; to create the .catkin_tools/profile/default/config.yaml file
    (call-process-shell-command (format "catkin config --init --workspace %s" ws))
    (message (format "Catkin workspace initialized successfully at '%s'" ws)))))

;;;###autoload
(defun helm-catkin-clean ()
  "Cleans the build/ devel/ and install/ folder for the catkin workspace at $EMACS_CATKIN_WS."
  (interactive)
  (let ((ws (getenv helm-catkin--WS)))
    (when (y-or-n-p (format "Clean workspace at '%s'? " ws))
      (call-process-shell-command (format "catkin clean --workspace %s -y" ws)))))

(defun helm-catkin--source (command)
  "Prepend a `source $EMACS_CATKIN_WS/devel/setup.bash &&' before COMMAND if such a file exists."
  (let* ((ws (getenv helm-catkin--WS))
         (setup-file (format "%s/devel/setup.bash" ws)))
    (if (file-exists-p setup-file)
        (format "source %s && %s" setup-file command)
      command)))

;;;###autoload
(defun helm-catkin-config-show ()
  "Print the current configuration of the catkin workspace at $EMACS_CATKIN_WS.
The config goes to a new buffer called *Catkin Config*. This can be dismissed by pressing `q'."
  (interactive)
  (helm-catkin--setup)
  (switch-to-buffer-other-window "*Catkin Config*")
  (erase-buffer)
  ;; Pipe stderr to null to supress "could not determine width" warning
  (call-process-shell-command (format "catkin --force-color config --workspace %s 2> /dev/null" (getenv helm-catkin--WS)) nil t)
  (xterm-color-colorize-buffer)
  (helm-catkin-mode))        ; set this buffer to be dissmissable with "Q"

;;;###autoload
(defun helm-catkin-config-open ()
  "Opens the config file for the catkin profile for the workspace at $EMACS_CATKIN_WS."
  (find-file (format "%s/.catkin_tools/profiles/default/config.yaml" (getenv helm-catkin--WS))))

(defun helm-catkin--config-args (operation &optional args)
  "Call 'catkin config' for the workspace to execute some OPERATION.
The ARGS are string joined with spaces and applied after the OPERATION.
This function can be used to set args of a certain type like so:

\(helm-catkin--config-args \"--cmake-args\"
                     '(\"-DCMAKE_ARG1=foo\" \"-DCMAKE_ARG2=bar\"))
\(helm-catkin--config-args \"--no-make-args\")"
  (unless (getenv helm-catkin--WS) (error (format "Catkin workspace at $%s not set. Have you called `helm-catkin-set-workspace'?" helm-catkin--WS)))
  (let ((arg-string (helm-catkin--util-format-list args " ")))
    (ignore-errors
      (substring
       (call-process-shell-command
        (format "catkin config --workspace %s %s %s"
                (getenv helm-catkin--WS)
                operation
                arg-string))
       0 -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  CMAKE Args                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-catkin-config-cmake-args ()
  "Return a list of all currenty set cmake args for the workspace at $EMACS_CATKIN_WS."
  (helm-catkin--parse-config "cmake_args"))

(defalias 'helm-catkin-config-cmake-args-clear (apply-partially 'helm-catkin--config-args "--no-cmake-args")
  "Removes all cmake args for the current workspace at $EMACS_CATKIN_WS")

(defalias 'helm-catkin-config-cmake-args-set (apply-partially 'helm-catkin--config-args "--cmake-args")
  "Sets a list of cmake args for the current workspace at $EMACS_CATKIN_WS. Passing an empty list to ARGS will clear all currently set args.")

(defalias 'helm-catkin-config-cmake-args-add (apply-partially 'helm-catkin--config-args "--append-args --cmake-args")
  "Adds a list of cmake args to the existing set of cmake args for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-cmake-args-remove (apply-partially 'helm-catkin--config-args "--remove-args --cmake-args")
  "Removes a list of cmake args from the existing set of cmake args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and are just ignored.")

(defun helm-catkin-config-cmake-change (arg)
  "Prompt the user to enter a new value for a CMake arg.
The prompt in the minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for CMake Arg: " arg)))
    (helm-catkin-config-cmake-args-remove (list arg))
    (helm-catkin-config-cmake-args-add (list new-arg))))

(defun helm-catkin-config-cmake-new (&optional _)
  "Prompts the user to enter a new CMake arg which will be returned."
  (interactive)
  (helm-catkin-config-cmake-args-add (list (helm-read-string "New CMake Arg: "))))

(defvar helm-catkin--helm-source-catkin-config-cmake
  (helm-build-sync-source "CMake"
    :candidates 'helm-catkin-config-cmake-args
    :help-message 'helm-catkin--helm-source-catkin-config-cmake-helm-message
    :action '(("Change" . (lambda (x) (helm-catkin-config-cmake-change x) (helm-catkin)))
              ("Add"    . (lambda (x) (helm-catkin-config-cmake-new x) (helm-catkin)))
              ("Clear"  . (lambda (_) (helm-catkin-config-cmake-args-remove (helm-marked-candidates)) (helm-catkin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   MAKE Args                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-catkin-config-make-args ()
  "Return a list of all currenty set make args for the workspace at $EMACS_CATKIN_WS."
  (append (helm-catkin--parse-config "make_args") (helm-catkin--parse-config "jobs_args")))

(defalias 'helm-catkin-config-make-args-clear (apply-partially 'helm-catkin--config-args "--no-make-args")
  "Removes all make args for the current workspace at $EMACS_CATKIN_WS.")
(defalias 'helm-catkin-config-make-args-set (apply-partially 'helm-catkin--config-args "--make-args")
  "Sets a list of make args for the current workspace at $EMACS_CATKIN_WS.
Passing an empty list to ARGS will clear all currently set args.")

(defalias 'helm-catkin-config-make-args-add (apply-partially 'helm-catkin--config-args "--append-args --make-args")
  "Add a list of make args to the existing set of make args for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-make-args-remove (apply-partially 'helm-catkin--config-args "--remove-args --make-args")
  "Remove a list of make args from the existing set of make args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and
are just ignored.")

(defun helm-catkin-config-make-change (arg)
  "Prompt the user to enter a new value for a Make arg.
The prompt in the minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for Make Arg: " arg)))
    (helm-catkin-config-make-args-remove (list arg))
    (helm-catkin-config-make-args-add (list new-arg))))

(defun helm-catkin-config-make-new (&optional _)
  "Prompt the user to enter a new Make arg which will be returned."
  (interactive)
  (helm-catkin-config-make-args-add (list (helm-read-string "New Make Arg: "))))

(defvar helm-catkin--helm-source-catkin-config-make
  (helm-build-sync-source "Make"
    :candidates 'helm-catkin-config-make-args
    :help-message 'helm-catkin--helm-source-catkin-config-make-helm-message
    :action '(("Change" . (lambda (x) (helm-catkin-config-make-change x) (helm-catkin)))
              ("Add"    . (lambda (x) (helm-catkin-config-make-new x) (helm-catkin)))
              ("Clear"  . (lambda (_) (helm-catkin-config-make-args-remove (helm-marked-candidates)) (helm-catkin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   CATKIN-MAKE Args                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-catkin-config-catkin-make-args ()
  "Return a list of all currenty set catkin-make args for the workspace at $EMACS_CATKIN_WS."
  (helm-catkin--parse-config "catkin_make_args"))

(defalias 'helm-catkin-config-catkin-make-args-clear (apply-partially 'helm-catkin--config-args "--no-catkin-make-args")
  "Remove all catkin-make args for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-catkin-make-args-set (apply-partially 'helm-catkin--config-args "--catkin-make-args")
  "Set a list of catkin-make args for the current workspace at $EMACS_CATKIN_WS.
Passing an empty list to ARGS will clear all currently set args.")

(defalias 'helm-catkin-config-catkin-make-args-add (apply-partially 'helm-catkin--config-args "--append-args --catkin-make-args")
  "Add a list of catkin-make args to the existing set of catkin-make args for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-catkin-make-args-remove (apply-partially 'helm-catkin--config-args "--remove-args --catkin-make-args")
  "Remove a list of catkin-make args from the existing set of catkin-make args for
the current workspace at $EMACS_CATKIN_WS. Args which are currently
not set and are requested to be removed don't provoce an error and
are just ignored.")

(defun helm-catkin-config-catkin-make-change (arg)
  "Prompt the user to enter a new value for a Catkin-Make arg.
The prompt in the minibuffer is autofilled with ARG and the new entered value will be returned."
  (interactive)
  (let ((new-arg (helm-read-string "Adjust value for Catkin-Make Arg: " arg)))
    (helm-catkin-config-catkin-make-args-remove (list arg))
    (helm-catkin-config-catkin-make-args-add (list new-arg))))

(defun helm-catkin-config-catkin-make-new (&optional _)
  "Prompt the user to enter a new Catkin-Make arg which will be returned."
  (interactive)
  (helm-catkin-config-catkin-make-args-add (list (helm-read-string "New Catkin-Make Arg: "))))

(defvar helm-catkin--helm-source-catkin-config-catkin-make
  (helm-build-sync-source "Catkin-Make"
    :candidates 'helm-catkin-config-catkin-make-args
    :help-message 'helm-catkin--helm-source-catkin-config-catkin-make-helm-message
    :action '(("Change" . (lambda (x) (helm-catkin-config-catkin-make-change x) (helm-catkin)))
              ("Add" . (lambda (x) (helm-catkin-config-catkin-make-new x) (helm-catkin)))
              ("Clear" . (lambda (_) (helm-catkin-config-catkin-make-args-remove (helm-marked-candidates)) (helm-catkin))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Whitelist/Blacklist                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun helm-catkin-config-whitelist ()
  "Return a list of all currenty whitelisted packages for the workspace at $EMACS_CATKIN_WS."
  (helm-catkin--parse-config "whitelist"))

(defalias 'helm-catkin-config-whitelist-add (apply-partially 'helm-catkin--config-args "--append-args --whitelist")
  "Mark a list of packages to be whitelisted for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-whitelist-remove (apply-partially 'helm-catkin--config-args "--remove-args --whitelist")
  "Remove a list of whitelisted packages from the existing whitelist for
the current workspace at $EMACS_CATKIN_WS. Packages which are currently
not whitelisted and are requested to be removed don't provoce an error and
are just ignored.")

(defvar helm-catkin--helm-source-catkin-config-whitelist
  (helm-build-sync-source "Whitelist"
    :candidates 'helm-catkin-config-whitelist
    :help-message 'helm-catkin--helm-source-catkin-config-whitelist-helm-message
    :action '(("Un-Whitelist" . (lambda (_) (helm-catkin-config-whitelist-remove (helm-marked-candidates)) (helm-catkin)))
              ("Build" . (lambda (_) (helm-catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . helm-catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (helm-catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (helm-catkin-open-pkg-package (helm-marked-candidates)))))))

(defun helm-catkin-config-blacklist ()
  "Return a list of all currenty blacklisted packages for the workspace at $EMACS_CATKIN_WS."
  (helm-catkin--parse-config "blacklist"))

(defalias 'helm-catkin-config-blacklist-add (apply-partially 'helm-catkin--config-args "--append-args --blacklist")
  "Mark a list of packages to be blacklisted for the current workspace at $EMACS_CATKIN_WS.")

(defalias 'helm-catkin-config-blacklist-remove (apply-partially 'helm-catkin--config-args "--remove-args --blacklist")
  "Remove a list of blacklisted packages from the existing blacklist for
the current workspace at $EMACS_CATKIN_WS. Packages which are currently
not blacklisted and are requested to be removed don't provoce an error and are just ignored.")

(defvar helm-catkin--helm-source-catkin-config-blacklist
  (helm-build-sync-source "Blacklist"
    :candidates 'helm-catkin-config-blacklist
    :help-message 'helm-catkin--helm-source-catkin-config-blacklist-help-message
    :action '(("Un-Blacklist" . (lambda (_) (helm-catkin-config-blacklist-remove (helm-marked-candidates)) (helm-catkin)))
              ("Build" . (lambda (_) (helm-catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . helm-catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (helm-catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (helm-catkin-open-pkg-package (helm-marked-candidates)))))))

(defvar helm-catkin--helm-source-catkin-config-new
  (helm-build-sync-source "[New]"
    :candidates '("CMake Arg" "Make Arg" "Catkin Make Arg")
    :help-message 'helm-catkin--helm-source-catkin-config-new-helm-message
    :action '(("Create New Arg" . (lambda (name)
                                    (cond ((string= name "CMake Arg")       (helm-catkin-config-cmake-new ""))
                                          ((string= name "Make Arg")        (helm-catkin-config-make-new ""))
                                          ((string= name "Catkin Make Arg") (helm-catkin-config-catkin-make-new ""))
                                          )
                                    (helm-catkin))))))

(defvar helm-catkin--helm-source-catkin-config-packages
  (helm-build-sync-source "Packages"
    :candidates 'helm-catkin-list
    :help-message 'helm-catkin--helm-source-catkin-config-packages-helm-message
    :action '(("Build" . (lambda (c) (helm-catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . helm-catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (helm-catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (helm-catkin-open-pkg-package (helm-marked-candidates))))
              ("Blacklist" . (lambda (_) (helm-catkin-config-blacklist-add (helm-marked-candidates)) (helm-catkin)))
              ("Whitelist" . (lambda (_) (helm-catkin-config-whitelist-add (helm-marked-candidates)) (helm-catkin))))))

(defvar helm-catkin-helm-help-string
  "* Catkin

Opens a helm query which shows the current config for the catkin workspace at $EMACS_CATKIN_WS.
It combines the different arguments into helm sections:

** Config Sections
When you fire up the `helm-catkin' command, you see a different sections listed below. If in one section
no argument is set, the section is omitted. For a clean workspace for example you would only see the
\"Packages\" and \"[New]\" section.

 - [New]           Add a new argument to the config
 - CMake:          The arguments passed to `cmake'
 - Make:           The arguments passed to `make'
 - Catkin Make:    The arguments passed to `catkin_make'
 - Whitelist:      Which packages are whitelisted, i.e. are build exclusively
 - Blacklisted:    Which packages are blacklisted, i.e. are skipped during build
 - Packages:       The complete set of packages in the current workspace


** Actions
Each section has a distinct set of actions for each item. Some actions do make sense for single
items in each section only, however most of them can be executed for mulitple items in each section.
The first action [F1] is always the default choice if you just press enter.

*** [New]    (any of these actions will re-show the helm-catkin dialog)
***** [F1] CMake Arg           :: add a new cmake argument to the config
***** [F2] Make Arg            :: add a new make argument to the config
***** [F3] Catkin Make Arg     :: add a new catkin_make argument to the config
*** CMake
***** [F1] Change              :: change the value of that cmake argument
***** [F2] Add                 :: add a new cmake argument
***** [F3] Clear               :: remove that/those selected cmake argument(s)
*** Make
***** [F1] Change              :: change the value of that make argument
***** [F2] Add                 :: add a new make argument
***** [F3] Clear               :: remove that/those selected make argument(s)
*** Catkin Make
***** [F1] Change              :: change the value of that catkin_make argument
***** [F2] Add                 :: add a new catkin_make argument
***** [F3] Clear               :: remove that/those selected catkin_make argument(s)
*** Blacklist
***** [F1] Un-Blacklist        :: remove the selected package(s) from the blacklist
***** [F2] Build               :: build the selected package(s)
***** [F3] Open Folder         :: open the package's folder in a `dired' buffer (no multi selection)
***** [F4] Open CMakeLists.txt :: open CMakeLists files of the selected package(s)
***** [F5] Open package.xml    :: open package files of the selected package(s)
*** Whitelist
***** [F1] Un-Whitelist        :: remove the selected package(s) from the whitelist
***** [F2] Build               :: build the selected package(s)
***** [F3] Open Folder         :: open the package's folder in a `dired' buffer (no multi selection)
***** [F4] Open CMakeLists.txt :: open CMakeLists files of the selected package(s)
***** [F5] Open package.xml    :: open package files of the selected package(s)
*** Packages
***** [F1] Build               :: build the selected package(s)
***** [F2] Open Folder         :: open the package's folder in a `dired' buffer (no multi selection)
***** [F3] Open CMakeLists.txt :: open CMakeLists files of the selected package(s)
***** [F4] Open package.xml    :: open package files of the selected package(s)
***** [F5] Blacklist           :: put the selected package(s) on the blacklist
***** [F6] Whitelist           :: put the selected package(s) on the whitelist

** Tips
**** Most of the actions above accept multiple items from that section.
**** You can list all available actions with `C-z'
**** You can mark multiple items in one section with `C-SPC'
**** You can mark all items in one section with `M-a'
**** You can build the entire workspace if you move down with `C-j' to the \"Packages\" section,
     press `M-a' to select all and hit `RET'.

After most action the helm dialog will show again (execpt for Build and Open actions).
To quit it just press ESC.")

(defvaralias 'helm-catkin--helm-source-catkin-config-cmake-helm-message       'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-make-helm-message        'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-catkin-make-helm-message 'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-whitelist-helm-message   'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-blacklist-helm-message   'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-packages-helm-message    'helm-catkin-helm-help-string)
(defvaralias 'helm-catkin--helm-source-catkin-config-new-helm-message         'helm-catkin-helm-help-string)

;;;###autoload
(defun helm-catkin ()
  "Helm command for catkin.
For more information use `C-h v helm-catkin-helm-help-message' or
press `C-c ?' in the helm-catkin helm query.

See `helm-catkin-helm-help-string'"
  (interactive)
  (helm-catkin--setup)
  (helm :buffer "*helm Catkin*"

        :sources '(helm-catkin--helm-source-catkin-config-new
                   helm-catkin--helm-source-catkin-config-cmake
                   helm-catkin--helm-source-catkin-config-make
                   helm-catkin--helm-source-catkin-config-catkin-make
                   helm-catkin--helm-source-catkin-config-whitelist
                   helm-catkin--helm-source-catkin-config-blacklist
                   helm-catkin--helm-source-catkin-config-packages)))

(defun helm-catkin--build-finished (process signal)
  "This get called, once the catkin build command finishes.
It marks the buffer as read-only and asks to close the window.
PROCESS is the process which runs the build command and SIGNAL
the signal with which the PROCESS finishes."
  (when (memq (process-status process) '(exit signal))
    (message "Catkin build done!")
    (other-window 1)      ; select the first "other" window, i.e. the build window
    (read-only-mode)      ; mark as not-editable
    (local-set-key (kbd "q") (lambda ()
                               (interactive)
                               (quit-window (get-buffer-window "*Catkin Build*"))))))

(defun helm-catkin-build-package (&optional pkgs)
  "Build the catkin workspace at $EMACS_CATKIN_WS after sourcing it's ws.
If PKGS is non-nil, only these packages are built, otherwise all packages in the ws are build."
  (let* ((packages (helm-catkin--util-format-list pkgs " "))
         (build-command (format "catkin build --workspace %s %s" (getenv helm-catkin--WS) packages))
         (buffer (get-buffer-create "*Catkin Build*"))
         (process (progn
                    (with-current-buffer "*Catkin Build*" (helm-catkin-mode))
                    (async-shell-command build-command buffer)
                    (get-buffer-process buffer))))
    (if (process-live-p process)
        (set-process-sentinel process #'helm-catkin--build-finished)
      (error "Could not attach process sentinel to \"catkin build\" since no such process is running"))))

(defun helm-catkin-list ()
  "Return a list of all packages in the workspace at $EMACS_CATKIN_WS."
  (helm-catkin--util-command-to-list
   (format "catkin list --workspace %s --unformatted --quiet" (getenv helm-catkin--WS))))

(defun helm-catkin-open-file-in (pkg file)
  "Open the file at `$(rospack find pkg)/file'.
PKG is the name of the ros package and FILE a relative path to it."
  (interactive)
  (helm-catkin--setup)
  (find-file (format "%s/%s" (helm-catkin--util-absolute-path-of pkg) file)))

(defun helm-catkin-open-pkg-cmakelist (pkgs)
  "Open the `CMakeLists.txt' file for each of the package names within PKGS."
  (loop for pkg in pkgs
        do (helm-catkin-open-file-in pkg "CMakeLists.txt")))

(defun helm-catkin-open-pkg-package (pkgs)
  "Open the `package.xml' file for each of the package names within PKGS."
  (loop for pkg in pkgs
        do (helm-catkin-open-file-in pkg "package.xml")))

(defun helm-catkin-open-pkg-dired (pkg)
  "Open the absolute path of PKG in `dired'."
  (interactive)
  (helm-catkin--setup)
  (dired (helm-catkin--util-absolute-path-of pkg)))

(defvar helm-catkin--helm-catkin-build-help-message
  "* Catkin build Help
Prompts the user via a helm dialog to select one or more
packages to build in the current workspace.

The first section specifies to build the default configuration
setup by the `helm-catkin' command. For example if you have black- or
whitelisted packages, building with \"[default]\" will take this
into account. Otherwise you can explicately select packages in
the second section, which should be build regardless of black-
and whitelist.

** Tips
**** To adjust the config from the build command use the [F2] on \"[default]\"
**** Most of the actions above accept multiple items from that section.
**** You can list all available actions with `C-z'
**** You can mark multiple items in one section with `C-SPC'
**** You can mark all items in the \"Packages\" section with `M-a'

** Actions:
**** [F1] Build:                Build the selected package(s)
**** [F2] Open Folder:          Open the package's folder in `dired' (no multiselection possible)
**** [F3] Open CMakeLists.txt   Open the `CMakeList.txt' file(s) in new buffer(s) for the selected package(s)
**** [F4] Open package.xml      Open the `package.xml' file(s) in new buffer(s) for the selected package(s)")

(defvar helm-catkin--helm-source-catkin-build-default-source
   (helm-build-sync-source "Config"
     :candidates '("[default]")
     :help-message 'helm-catkin--helm-catkin-build-help-message
     :action '(("Build" . (lambda (_) (helm-catkin-build-package)))
               ("Open Config" . (lambda (_) (helm-catkin))))))

(defvar helm-catkin--helm-source-catkin-build-source
  (helm-build-sync-source "Packages"
    :candidates 'helm-catkin-list
    :help-message 'helm-catkin--helm-catkin-build-help-message
    :action '(("Build"               . (lambda (c) (helm-catkin-build-package (helm-marked-candidates))))
              ("Open Folder"         . helm-catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (helm-catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml"    . (lambda (c) (helm-catkin-open-pkg-package (helm-marked-candidates)))))))

;;;###autoload
(defun helm-catkin-build ()
  "Prompt the user via a helm dialog to select one or more packages to build.

  See `helm-catkin-helm-help-string'"
 (interactive)
  (helm-catkin--setup)
  (helm :buffer "*helm Catkin Build*"
        :sources '(helm-catkin--helm-source-catkin-build-default-source
                   helm-catkin--helm-source-catkin-build-source)))

(provide 'helm-catkin)

;;; helm-catkin.el ends here
