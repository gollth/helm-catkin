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
      (ignore-errors (split-string (substring (buffer-string) 0 -1) sep t))
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
      (catkin--set-ws (read-directory-name "Set catkin workspace: " (getenv catkin--WS)))
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
Check the value of CMAKE_PREFIX_PATH with `setenv' and/or call `catkin-set-workspace' with a path to your workspace (e.g. \"/opt/ros/kinetic\")"))
          (t (loop for path in (split-string cmake-prefix-path ":")
                 if (file-exists-p (format "%s/.catkin" path))
                 do (setenv catkin--WS (format "%s/.." path))
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
  "Prepends a `source $EMACS_CATKIN_WS/devel/setup.bash &&' before COMMAND if such a file exists."
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

;;;###autoload
(defun catkin-config-open ()
  "Opens the config file for the catkin profile for the workspace at $EMACS_CATKIN_WS"
  (find-file (format "%s/.catkin_tools/profiles/default/config.yaml" (getenv catkin--WS)))
  )

(defun catkin--config-args (operation &optional args)
  "Calls 'catkin config' for the workspace at $EMACS_CATKIN_WS to execute some OPERATION.
The ARGS are string joined with spaces and applied after the OPERATION. This function
can be used to set args of a certain type like so:

(catkin--config-args \"--cmake-args\" '(\"-DCMAKE_ARG1=foo\" \"-DCMAKE_ARG2=bar\"))
(catkin--config-args \"--no-make-args\")
"
  (unless (getenv catkin--WS) (error (format "Catkin workspace at $%s not set. Have you called `catkin-set-workspace'?" catkin--WS)))
  (let ((arg-string (catkin--util-format-list args " ")))
    (ignore-errors
      (substring
       (call-process-shell-command
        (format "catkin config --workspace %s %s %s" (getenv catkin--WS) operation arg-string)
        )
       0 -1)
      )
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
(defvar helm-source-catkin-config-cmake
  (helm-build-sync-source "CMake"
    :candidates 'catkin-config-cmake-args
    :help-message 'helm-source-catkin-config-cmake-helm-message
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
  (append (catkin--parse-config "make_args") (catkin--parse-config "jobs_args"))
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
(defvar helm-source-catkin-config-make
  (helm-build-sync-source "Make"
    :candidates 'catkin-config-make-args
    :help-message 'helm-source-catkin-config-make-helm-message
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

(defvar helm-source-catkin-config-catkin-make
  (helm-build-sync-source "Catkin-Make"
    :candidates 'catkin-config-catkin-make-args
    :help-message 'helm-source-catkin-config-catkin-make-helm-message
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
(defvar helm-source-catkin-config-whitelist
  (helm-build-sync-source "Whitelist"
    :candidates 'catkin-config-whitelist
    :help-message 'helm-source-catkin-config-whitelist-helm-message
    :action '(("Un-Whitelist" . (lambda (_) (catkin-config-whitelist-remove (helm-marked-candidates)) (catkin)))
              ("Build" . (lambda (_) (catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates)))))
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
(defvar helm-source-catkin-config-blacklist
  (helm-build-sync-source "Blacklist"
    :candidates 'catkin-config-blacklist
    :help-message 'helm-source-catkin-config-blacklist-help-message
    :action '(("Un-Blacklist" . (lambda (_) (catkin-config-blacklist-remove (helm-marked-candidates)) (catkin)))
              ("Build" . (lambda (_) (catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates)))))
    )
  )

(defvar helm-source-catkin-config-new
  (helm-build-sync-source "[New]"
    :candidates '("CMake Arg" "Make Arg" "Catkin Make Arg")
    :help-message 'helm-source-catkin-config-new-helm-message
    :action '(("Create New Arg" . (lambda (name)
                                    (cond ((string= name "CMake Arg") (catkin-config-cmake-new ""))
                                          ((string= name "Make Arg") (catkin-config-make-new ""))
                                          ((string= name "Catkin Make Arg") (catkin-config-catkin-make-new ""))
                                          )
                                    (catkin)
                                    )
               ))
    )
  )

(defvar helm-source-catkin-config-packages
  (helm-build-sync-source "Packages"
    :candidates 'catkin-list
    :help-message 'helm-source-catkin-config-packages-helm-message
    :action '(("Build" . (lambda (c) (catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates))))
              ("Blacklist" . (lambda (_) (catkin-config-blacklist-add (helm-marked-candidates)) (catkin)))
              ("Whitelist" . (lambda (_) (catkin-config-whitelist-add (helm-marked-candidates)) (catkin))))
    )
  )

(defvar helm-catkin-help-string
  "* Catkin

Opens a helm query which shows the current config for the catkin workspace at $EMACS_CATKIN_WS.
It combines the different arguments into helm sections:

** Config Sections
When you fire up the `catkin' command, you see a different sections listed below. If in one section
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

*** [New]    (any of these actions will re-show the catkin dialog)
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

(defvaralias 'helm-source-catkin-config-cmake-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-make-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-catkin-make-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-whitelist-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-blacklist-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-packages-helm-message 'helm-catkin-help-string)
(defvaralias 'helm-source-catkin-config-new-helm-message 'helm-catkin-help-string)

;;;###autoload
(defun catkin ()
  "Helm command for catkin. For more information use `C-h v helm-catkin-help-message' or
press `C-c ?' in the catkin helm query.

See `helm-catkin-help-string'
"
 (interactive)
  (catkin--setup)
  (helm :buffer "*helm Catkin*"

        :sources '(helm-source-catkin-config-new
                   helm-source-catkin-config-cmake
                   helm-source-catkin-config-make
                   helm-source-catkin-config-catkin-make
                   helm-source-catkin-config-whitelist
                   helm-source-catkin-config-blacklist
                   helm-source-catkin-config-packages
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
    (local-set-key (kbd "q") (lambda () (interactive) (kill-this-buffer) (delete-window)))
    )
  )

(defun catkin-build-package (&optional pkgs)
  "Build the catkin workspace at $EMACS_CATKIN_WS after sourcing it's ws. If PKGS is non-nil, only these packages are built, otherwise all packages in the ws are build"
  (let* ((packages (catkin--util-format-list pkgs " "))
         (build-command (format "catkin build --workspace %s %s" (getenv catkin--WS) packages))
         (buffer (get-buffer-create "*Catkin Build*"))
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

(defvar helm-catkin-build-help-message
  "* Catkin build Help
Prompts the user via a helm dialog to select one or more
packages to build in the current workspace.

** Tips
**** Most of the actions above accept multiple items from that section.
**** You can list all available actions with `C-z'
**** You can mark multiple items in one section with `C-SPC'
**** You can mark all items in one section with `M-a'

** Actions:
**** [F1] Build:                Build the selected package(s)
**** [F2] Open Folder:          Open the package's folder in `dired' (no multiselection possible)
**** [F3] Open CMakeLists.txt   Open the `CMakeList.txt' file(s) in new buffer(s) for the selected package(s)
**** [F4] Open package.xml      Open the `package.xml' file(s) in new buffer(s) for the selected package(s)
")
(defvar helm-source-catkin-build-source
  (helm-build-sync-source "Packages"
    :candidates 'catkin-list
    :help-message 'helm-catkin-build-help-message
    :action '(("Build" . (lambda (c) (catkin-build-package (helm-marked-candidates))))
              ("Open Folder" . catkin-open-pkg-dired)
              ("Open CMakeLists.txt" . (lambda (c) (catkin-open-pkg-cmakelist (helm-marked-candidates))))
              ("Open package.xml" . (lambda (c) (catkin-open-pkg-package (helm-marked-candidates))))
              )
    ))

;;;###autoload
(defun catkin-build ()
"Prompts the user via a helm dialog to select one or more
  packages to build in the current workspace.

  See `helm-catkin-build-help-message'"
 (interactive)
  (catkin--setup)
  (helm :buffer "*helm Catkin Build*"
        :sources 'helm-source-catkin-build-source
        )
  )

(provide 'catkin)

;;; catkin.el ends here
