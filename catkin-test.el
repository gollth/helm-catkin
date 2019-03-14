(load-file "catkin.el")
(require 'el-mock)


(ert-deftest test-catkin--util-format-list-returns-string ()
  "Tests if the catkin--util-format-list function returns a string for given inputs"
  (should (stringp (catkin--util-format-list nil " ")))
  (should (stringp (catkin--util-format-list '("Hello" "World") " ")))
  (should (string= (catkin--util-format-list '("Hello" "World") " ") "Hello World"))
  )


(ert-deftest test-catkin--util-command-to-list-returns-list-of-strings ()
  "Test if the catkin--util-command-to-list function returns a list of strings."
  (let* ((expected-data '("hello" "world"))
         (test-command "echo 'hello\nworld'")
         (data (catkin--util-command-to-list test-command)))
    (should (sequencep data))
    (should (equal data expected-data))
    )
  )

(ert-deftest test-catkin--util-command-to-list-returns-list-of-strings-with-custom-sep ()
  "Test if the catkin--util-command-to-list function returns a list of strings for a non-default separator."
  (let* ((expected-data '("hello" "world"))
         (test-command "echo 'hello-world'")
         (data (catkin--util-command-to-list test-command "-")))
    (should (sequencep data))
    (should (equal data expected-data))
    )
  )

(defconst path (file-name-directory (or load-file-name buffer-file-name)))
(defconst npath (format "%s/path/which/does/not/exist" path))

(ert-deftest test-catkin--parse-config-raises-without-initialized-workspace ()
  "Test if the parse-config command throws an error when the ws is not initialized"
  (with-mock
    (mock (getenv catkin--WS) => npath)
    (should-error (catkin--parse-config nil))
    )
  )

(ert-deftest test-catkin--parse-config-key-with-simple-value ()
  "Test if a keyword with one value like 'null' or 'false' is returned as string"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (string= "false" (catkin--parse-config "install")))
    )
  )
(ert-deftest test-catkin--parse-config-key-with-list-value ()
  "Test if a keyword with multiple values like 'cmake_args' are returned as list of strings"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                   (catkin--parse-config "cmake_args")
                   )
            )
    )
  )
(ert-deftest test-catkin--parse-config--key-with-empty-list-value ()
  "Test if a keyword with an empty list '[]' (yaml syntax) is returned as empty list '() (lisp syntax)"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should-not (catkin--parse-config "job_args"))
    )
  )

(ert-deftest test-catkin--parse-config-with-unknown-key-returns-nil ()
  "Test if an unknown keyword is returned as nil and does not raise an error"
  (with-mock
   (mock (getenv catkin--WS) => path)
   (should-not (catkin--parse-config "blubiblub"))
   )
  )

(ert-deftest test-catkin--set-ws-without-args-looks-into-cmake-prefix-path ()
  "Calling catkin--set-ws without explicit path argument makes the function read the value from CMAKE_PREFIX_PATH"
  (with-mock
    (mock (getenv "CMAKE_PREFIX_PATH") => path :times 1)
    (mock (setenv catkin--WS path) :times 1)
    (should-not (catkin--set-ws))  ;; returns nil on success
    )
  )

(ert-deftest test-catkin--set-ws-without-args-and-without-cmake-prefix-path-raises ()
  "If no CMAKE_PREFIX_PATH is set and automatic catkin extraction is requested"
  (with-mock
    (mock (getenv "CMAKE_PREFIX_PATH"))  ;; returns nil
    (should-error (catkin--set-ws))
    )
  )

(ert-deftest test-catkin--set-ws-with-args-sets-the-env ()
  "Calling catkin--set-ws with an explicit path will set the env accordingly."
  (with-mock
    (mock (setenv catkin--WS "/test/path") :times 1)
    (catkin--set-ws "/test/path")
    )
  )

(ert-deftest test-catkin-config-cmake-args-are-correct ()
  "Calling catkin-config-cmake-args returns the values from the config.yaml file"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal (catkin-config-cmake-args) '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")))
    )
  )
(ert-deftest test-catkin-config-make-args-are-correct ()
  "Calling catkin-config-make-args returns the values from the config.yaml file"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal (catkin-config-make-args) '("-j4")))
    )
  )

(ert-deftest test-catkin-config-catkin-make-args-are-correct ()
  "Calling catkin-config-catkin-make-args returns the values from the config.yaml file"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal (catkin-config-catkin-make-args) '()))
    )
  )

(ert-deftest test-catkin-config-blacklist-is-correct ()
  "Calling catkin-config-blacklist returns the packages from the config.yaml file"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal (catkin-config-blacklist) '("pkg1" "pkg2" "pkg3")))
    )
  )

(ert-deftest test-catkin-config-whitelist-is-correct ()
  "Calling catkin-config-whitelist returns the packages from the config.yaml file"
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal (catkin-config-whitelist)'()))
    )
  )

(provide 'catkin-test)
