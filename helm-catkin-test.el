(load-file "helm-catkin.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Helper functions for Testing & Util tests                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst path (file-name-directory (or load-file-name buffer-file-name)))
(defconst npath "/path/which/does/not/exist")

(defun test-helper-non-empty (s)
  "Returns nil if S is empty, otherwise returns S."
  (if (string= s "") nil s))

(defun test-helper-diff (filters)
  "Compares the 'config.yaml' file with the 'config.yaml.bak' file and `grep's for a custom FILTERS.
The FILTERS are anded in the grep search and the order is important! Note that the `diff' command
first outputs removes (\"< ...\") and then additions (\"> ... \"). Returns the resulting matched string
of nil if no match. Can be used to test if a certain change was made between the two files:
(test-helper-diff '(\"> - NEW_CMAKE_ARG\"))   ;; a new cmake arg has been added to config.yaml
(test-helper-diff '(\"< extend_path: null\")) ;; the extend_path: null was removed from config.yaml
(test-helper-diff '(\"< args: []\"
                    \"> - -Value1\"))         ;; 'args' changed from empty list to one element"
  (test-helper-non-empty
   (shell-command-to-string
    (format "diff %s/.catkin_tools/profiles/default/config.yaml.bak %s/.catkin_tools/profiles/default/config.yaml | grep -vsE '^[0-9-].*$' | grep -soF '%s' "
            path path (helm-catkin--util-format-list filters "\n")))))

(defun test-helper-backup ()
  (copy-file (format "%s/.catkin_tools/profiles/default/config.yaml" path)
             (format "%s/.catkin_tools/profiles/default/config.yaml.bak" path) t))

(defun test-helper-unbackup ()
  (copy-file (format "%s/.catkin_tools/profiles/default/config.yaml.bak" path) (format "%s/.catkin_tools/profiles/default/config.yaml" path) t)
  (delete-file (format "%s/.catkin_tools/profiles/default/config.yaml.bak" path)))

(ert-deftest test-helm-catkin--util-format-list-returns-string ()
  "Test if the helm-catkin--util-format-list function returns a string for given inputs."
  (should (stringp (helm-catkin--util-format-list nil " ")))
  (should (stringp (helm-catkin--util-format-list '("Hello" "World") " ")))
  (should (string= (helm-catkin--util-format-list '("Hello" "World") " ") "Hello World")))


(ert-deftest test-helm-catkin--util-command-to-list-returns-list-of-strings ()
  "Test if the helm-catkin--util-command-to-list function returns a list of strings."
  (let* ((expected-data '("hello" "world"))
         (test-command "echo 'hello\nworld'")
         (data (helm-catkin--util-command-to-list test-command)))
    (should (sequencep data))
    (should (equal data expected-data))))

(ert-deftest test-helm-catkin--util-command-to-list-returns-list-of-strings-with-custom-sep ()
  "Test if the helm-catkin--util-command-to-list function returns a list of strings for a non-default separator."
  (let* ((expected-data '("hello" "world"))
         (test-command "echo 'hello-world'")
         (data (helm-catkin--util-command-to-list test-command "-")))
    (should (sequencep data))
    (should (equal data expected-data))))

(ert-deftest test-helm-catkin--util-absolute-path-of-raises-for-unknown-package ()
  "Test if the helm-catkin--util-absolute-path function throws an error for unknown packages."
  (setq helm-catkin-workspace path)
  (should-error (helm-catkin--util-absolute-path-of "some_unknown_package")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Workspace Tests                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest test-helm-catkin--get-workspace-returns-path-to-workspace ()
  "For the root of the workspace helm-catkin--get-workspace returns the correct value"
  (setq helm-catkin-workspace path)
  (should (string-prefix-p (helm-catkin--get-workspace) path)))

(ert-deftest test-helm-catkin--get-workspace-returns-top-level-path ()
  "For a subpath helm-catkin--get-workspace-returns the root of the workspace."
  (setq helm-catkin-workspace (format "%ssome/file" path))
  (should (string-prefix-p (helm-catkin--get-workspace) path)))

(ert-deftest test-helm-catkin--get-workspace-raises-for-invalid-workspace ()
  "Test if the helm-catkin--get-workspace function throws an error for non-catkin folders."
  (setq helm-catkin-workspace npath)
  (should-error (helm-catkin--get-workspace)))

(ert-deftest test-helm-catkin--is-workspace-initialized-returns-t-for-valid-ws ()
  "Initialized workspaces are recognized as such."
  (should (helm-catkin--is-workspace-initialized path)))

(ert-deftest test-helm-catkin--is-workspace-initialized-returns-nil-for-invalid-ws ()
  "Not initialized workspaces are recognized as such."
  (should-not (helm-catkin--is-workspace-initialized npath)))

(ert-deftest test-helm-catkin--get-workspace-for-nil-global-var-returns-buffer-directory ()
  "If the global variable `helm-catkin-workspace' is not set, test if the buffer path is returned"
  (unwind-protect (progn
                    (setq helm-catkin-workspace nil)
                    (make-directory "/tmp/ws/" t)
                    (write-region "" nil "/tmp/ws/some-file")
                    (helm-catkin-init "/tmp/ws/")
                    (find-file "/tmp/ws/some-file")
                    (with-current-buffer "some-file"
                      (should (string= "/tmp/ws" (helm-catkin--get-workspace)))))
    (when (buffer-live-p "some-file") (kill-buffer "some-file"))
    (when (file-exists-p "/tmp/ws") (delete-directory "/tmp/ws" t))))

(ert-deftest test-helm-catkin--parse-config-raises-without-initialized-workspace ()
  "Test if the parse-config command throws an error when the ws is not initialized"
  (setq helm-catkin-workspace npath)
  (should-error (helm-catkin--parse-config nil)))

(ert-deftest test-helm-catkin--parse-config-key-with-simple-value ()
  "Test if a keyword with one value like 'null' or 'false' is returned as string."
  (setq helm-catkin-workspace path)
  (should (string= "false" (helm-catkin--parse-config "install"))))

(ert-deftest test-helm-catkin--parse-config-key-with-list-value ()
  "Test if a keyword with multiple values like 'cmake_args' are returned as list of strings."
  (setq helm-catkin-workspace path)
  (should (equal '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                 (helm-catkin--parse-config "cmake_args"))))

(ert-deftest test-helm-catkin--parse-config--key-with-empty-list-value ()
  "Test if a keyword with an empty list '[]' (yaml syntax) is returned as empty list '() (lisp syntax)."
  (setq helm-catkin-workspace path)
  (should-not (helm-catkin--parse-config "whitelist")))  ;; in config whitelist is explicitly kept emtpy for testing

(ert-deftest test-helm-catkin--parse-config-with-unknown-key-returns-nil ()
  "Test if an unknown keyword is returned as nil and does not raise an error."
  (setq helm-catkin-workspace path)
  (should-not (helm-catkin--parse-config "blubiblub")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              CMake Arg Tests                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest test-helm-catkin-config-cmake-args-are-correct ()
  "Calling helm-catkin-config-cmake-args returns the values from the config.yaml file."
  (setq helm-catkin-workspace path)
  (should (equal (helm-catkin-config-cmake-args)
                 '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))))

(ert-deftest test-helm-catkin-config-cmake-args-add ()
  "Calling helm-catkin-config-cmake-args-add will put an entry into the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-cmake-args-add (list "New_CMake_Arg"))
        (should (test-helper-diff '("> - New_CMake_Arg"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-cmake-args-remove ()
  "Calling helm-catkin-config-cmake-args-remove will delete an entry from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-cmake-args-remove '("-DCMAKE_BUILD_TYPE=Release"))
        (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-cmake-args-clear ()
  "Calling helm-catkin-config-cmake-args-clear will remove all entries from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-cmake-args-clear)
        ;; The two cmake ares are now missing and a new line with : [] exists
        (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"
                                    "< - -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                    "> cmake_args: []"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-cmake-args-set ()
  "Calling helm-catkin-config-cmake-args-set will set the list regardless of its previous value."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-cmake-args-set '("Value1" "Value2"))
        (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"
                                    "< - -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                    "> - Value1"
                                    "> - Value2"))))
    (test-helper-unbackup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Make Arg Tests                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest test-helm-catkin-config-make-args-are-correct ()
  "Calling helm-catkin-config-make-args returns the values from the config.yaml file.
Make args are a bit special, because they can be in job_args or make_args key."
  (setq helm-catkin-workspace path)
  (should (equal (helm-catkin-config-make-args) '("-some_make_arg" "-j4"))))

(ert-deftest test-helm-catkin-config-make-args-add ()
  "Calling helm-catkin-config-make-args-add will put an entry into the config.yaml file"
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-make-args-add (list "New_Make_Arg"))
        (should (test-helper-diff '("> - New_Make_Arg"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-make-args-remove ()
  "Calling helm-catkin-config-make-args-remove will delete an entry from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-make-args-remove (list "-some_make_arg"))
        (should (test-helper-diff '("< - -some_make_arg"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-make-args-clear ()
  "Calling helm-catkin-config-make-args-clear will remove all entries from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-make-args-clear)
        ;; The two make arg lists are now missing and new lines with : [] exists
        (should (test-helper-diff '("< - -j4"
                                    "< - -some_make_arg"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-make-args-set ()
  "Calling helm-catkin-config-make-args-set will set the list regardless of its previous value."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-make-args-set '("Value1" "Value2"))
        (should (test-helper-diff '("< - -j4"
                                    "> - Value1"
                                    "> - Value2"))))
    (test-helper-unbackup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Catkin Make Arg Tests                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest test-helm-catkin-config-catkin-make-args-are-correct ()
  "Calling helm-catkin-config-catkin-make-args returns the values from the config.yaml file."
  (setq helm-catkin-workspace path)
  (should (equal (helm-catkin-config-catkin-make-args) '())))

(ert-deftest test-helm-catkin-config-catkin-make-args-add ()
  "Calling helm-catkin-config-catkin-make-args-add will put an entry into the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-catkin-make-args-add (list "New_Catkin_Make_Arg"))
        (should (test-helper-diff '("> - New_Catkin_Make_Arg"))))
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-catkin-make-args-remove ()
  "Calling helm-catkin-config-catkin-make-args-remove will delete an entry from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-catkin-make-args-remove '("unknown catkin-make-arg"))
        (should-not (test-helper-diff '(""))))   ;; no change
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-catkin-make-args-clear ()
  "Calling helm-catkin-config-catkin-make-args-clear will remove all entries from the config.yaml file."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-catkin-make-args-clear)
        (should-not (test-helper-diff '(""))))   ;; no change
    (test-helper-unbackup)))

(ert-deftest test-helm-catkin-config-catkin-make-args-set ()
  "Calling helm-catkin-config-catkin-make-args-set will set the list regardless of its previous value."
  (setq helm-catkin-workspace path)
  (test-helper-backup)
  (unwind-protect   ;; make sure the file is restored if an error occurs
      (progn
        (helm-catkin-config-catkin-make-args-set '("Value1" "Value2"))
        (should (test-helper-diff '("< catkin_make_args: []"
                                    "> catkin_make_args:"
                                    "> - Value1"
                                    "> - Value2"))))
    (test-helper-unbackup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              White/Black list Tests                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ert-deftest test-helm-catkin-config-blacklist-is-correct ()
  "Calling helm-catkin-config-blacklist returns the packages from the config.yaml file."
  (setq helm-catkin-workspace path)
  (should (equal (helm-catkin-config-blacklist) '("pkg1" "pkg2" "pkg3"))))

(ert-deftest test-helm-catkin-config-whitelist-is-correct ()
  "Calling helm-catkin-config-whitelist returns the packages from the config.yaml file."
  (setq helm-catkin-workspace path)
  (should (equal (helm-catkin-config-whitelist)'())))


(provide 'helm-catkin-test)
