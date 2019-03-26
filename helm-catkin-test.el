(load-file "helm-catkin.el")
(require 'el-mock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for Testing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst path (file-name-directory (or load-file-name buffer-file-name)))
(defconst npath (format "%s/path/which/does/not/exist" path))

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


(ert-deftest test-helm-catkin-workspace-returns-path ()
  "Test if the helm-catkin-workspace function returns a string to the current workspace."
  (with-mock
   (mock (getenv helm-catkin--WS) => path)
   (should (string= path (helm-catkin-workspace)))))

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
  "Test if the catkin--util-absolute-path function throws an error for unknown packages."
  (with-mock
   (mock (getenv helm-catkin--WS) => path)
   (should-error (helm-catkin--util-absolute-path-of "some_unknown_package"))))

(ert-deftest test-helm-catkin--parse-config-raises-without-initialized-workspace ()
  "Test if the parse-config command throws an error when the ws is not initialized"
  (with-mock
    (mock (getenv helm-catkin--WS) => npath)
    (should-error (helm-catkin--parse-config nil))))

(ert-deftest test-helm-catkin--parse-config-key-with-simple-value ()
  "Test if a keyword with one value like 'null' or 'false' is returned as string."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (string= "false" (helm-catkin--parse-config "install")))))

(ert-deftest test-helm-catkin--parse-config-key-with-list-value ()
  "Test if a keyword with multiple values like 'cmake_args' are returned as list of strings."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                   (helm-catkin--parse-config "cmake_args")))))

(ert-deftest test-helm-catkin--parse-config--key-with-empty-list-value ()
  "Test if a keyword with an empty list '[]' (yaml syntax) is returned as empty list '() (lisp syntax)."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should-not (helm-catkin--parse-config "whitelist"))))  ;; in config whitelist is explicitly kept emtpy for testing

(ert-deftest test-helm-catkin--parse-config-with-unknown-key-returns-nil ()
  "Test if an unknown keyword is returned as nil and does not raise an error."
  (with-mock
   (mock (getenv helm-catkin--WS) => path)
   (should-not (helm-catkin--parse-config "blubiblub"))))

(ert-deftest test-helm-catkin--set-ws-without-args-looks-into-cmake-prefix-path ()
  "Calling helm-catkin--set-ws without explicit path argument makes the function read the value from $CMAKE_PREFIX_PATH."
  (let ((cmake-prefix-path (format "%sdevel" path))
        (catkin-marker (format "%sdevel/.catkin" path)))
    (with-mock
      (mock (getenv "CMAKE_PREFIX_PATH") => cmake-prefix-path :times 1)
      (mock (file-exists-p catkin-marker) => t)
      (mock (setenv helm-catkin--WS *) :times 1)
      (should-not (helm-catkin--set-ws)))))  ;; returns nil on success

(ert-deftest test-helm-catkin--set-ws-without-args-and-without-cmake-prefix-path-raises ()
  "If no CMAKE_PREFIX_PATH is set and automatic helm-catkin extraction is requested."
  (with-mock
    (mock (getenv "CMAKE_PREFIX_PATH"))  ;; returns nil
    (should-error (helm-catkin--set-ws))))

(ert-deftest test-helm-catkin--set-ws-with-args-sets-the-env ()
  "Calling helm-catkin--set-ws with an explicit path will set the env accordingly."
  (with-mock
    (mock (setenv helm-catkin--WS "/test/path") :times 1)
    (helm-catkin--set-ws "/test/path")))

(ert-deftest test-helm-catkin--config-args-raises-if-env-not-set ()
  "Calling helm-catkin--config-args-find without prior setting of the env will throw an error"
  (with-mock
    (mock (getenv helm-catkin--WS) => nil)
    (should-error (helm-catkin--config-args ""))))      ;; no operation

;; CMake Arg Test ;;
(ert-deftest test-helm-catkin-config-cmake-args-are-correct ()
  "Calling helm-catkin-config-cmake-args returns the values from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal (helm-catkin-config-cmake-args)
                   '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")))))

(ert-deftest test-helm-catkin-config-cmake-args-add ()
  "Calling helm-catkin-config-cmake-args-add will put an entry into the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-cmake-args-add (list "New_CMake_Arg"))
          (should (test-helper-diff '("> - New_CMake_Arg"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-cmake-args-remove ()
  "Calling helm-catkin-config-cmake-args-remove will delete an entry from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-cmake-args-remove '("-DCMAKE_BUILD_TYPE=Release"))
          (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-cmake-args-clear ()
  "Calling helm-catkin-config-cmake-args-clear will remove all entries from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-cmake-args-clear)
          ;; The two cmake ares are now missing and a new line with : [] exists
          (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"
                                      "< - -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                      "> cmake_args: []"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-cmake-args-set ()
  "Calling helm-catkin-config-cmake-args-set will set the list regardless of its previous value."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-cmake-args-set '("Value1" "Value2"))
          (should (test-helper-diff '("< - -DCMAKE_BUILD_TYPE=Release"
                                      "< - -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                                      "> - Value1"
                                      "> - Value2"))))
      (test-helper-unbackup))))

;; Make args Tests ;;
(ert-deftest test-helm-catkin-config-make-args-are-correct ()
  "Calling helm-catkin-config-make-args returns the values from the config.yaml file.
Make args are a bit special, because they can be in job_args or make_args key."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal (helm-catkin-config-make-args) '("-some_make_arg" "-j4")))))

(ert-deftest test-helm-catkin-config-make-args-add ()
  "Calling helm-catkin-config-make-args-add will put an entry into the config.yaml file"
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-make-args-add (list "New_Make_Arg"))
          (should (test-helper-diff '("> - New_Make_Arg"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-make-args-remove ()
  "Calling helm-catkin-config-make-args-remove will delete an entry from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-make-args-remove '("-j4"))
          (should (test-helper-diff '("< - -j4"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-make-args-clear ()
  "Calling helm-catkin-config-make-args-clear will remove all entries from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-make-args-clear)
          ;; The two make arg lists are now missing and new lines with : [] exists
          (should (test-helper-diff '("< - -j4"
                                      "< - -some_make_arg"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-make-args-set ()
  "Calling helm-catkin-config-make-args-set will set the list regardless of its previous value."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-make-args-set '("Value1" "Value2"))
          (should (test-helper-diff '("< - -j4"
                                      "> - Value1"
                                      "> - Value2"))))
      (test-helper-unbackup))))

;; Catkin-Make args Tests ;;
(ert-deftest test-helm-catkin-config-catkin-make-args-are-correct ()
  "Calling helm-catkin-config-catkin-make-args returns the values from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal (helm-catkin-config-catkin-make-args) '()))))

(ert-deftest test-helm-catkin-config-catkin-make-args-add ()
  "Calling helm-catkin-config-catkin-make-args-add will put an entry into the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-catkin-make-args-add (list "New_Catkin_Make_Arg"))
          (should (test-helper-diff '("> - New_Catkin_Make_Arg"))))
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-catkin-make-args-remove ()
  "Calling helm-catkin-config-catkin-make-args-remove will delete an entry from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-catkin-make-args-remove '("unknown catkin-make-arg"))
          (should-not (test-helper-diff '(""))))   ;; no change
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-catkin-make-args-clear ()
  "Calling helm-catkin-config-catkin-make-args-clear will remove all entries from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-catkin-make-args-clear)
          (should (test-helper-diff '("> jobs_args: []"))))   ;; clears jobs_args
      (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-catkin-make-args-set ()
  "Calling helm-catkin-config-catkin-make-args-set will set the list regardless of its previous value."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (test-helper-backup)
    (unwind-protect   ;; make sure the file is restored if an error occurs
        (progn
          (helm-catkin-config-catkin-make-args-set '("Value1" "Value2"))
          (should (test-helper-diff '("< catkin_make_args: []"
                                      "> catkin_make_args:"
                                      "> - Value1"
                                      "> - Value2"))))
     (test-helper-unbackup))))

(ert-deftest test-helm-catkin-config-blacklist-is-correct ()
  "Calling helm-catkin-config-blacklist returns the packages from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal (helm-catkin-config-blacklist) '("pkg1" "pkg2" "pkg3")))))

(ert-deftest test-helm-catkin-config-whitelist-is-correct ()
  "Calling helm-catkin-config-whitelist returns the packages from the config.yaml file."
  (with-mock
    (mock (getenv helm-catkin--WS) => path)
    (should (equal (helm-catkin-config-whitelist)'()))))

(provide 'helm-catkin-test)
