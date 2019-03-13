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
(ert-deftest test-catkin--parse-config-raises-without-initialized-workspace ()
  (with-mock
    (mock (getenv catkin--WS) => (format %s "path/which/does/never/exist"))
    (should-error (catkin--parse-config nil))
    )
  )

(ert-deftest test-catkin--parse-config-with-simple-keywords ()
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (string= "false" (catkin--parse-config "install")))
    )
  )
(ert-deftest test-catkin--parse-config-with-list-property ()
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                   (catkin--parse-config "cmake_args")
                   )
            )
    )
  )
(ert-deftest test-catkin--parse-config-with-empty-list ()
  (with-mock
    (mock (getenv catkin--WS) => path)
    (should (equal '() (catkin--parse-config "blacklist"))
            )
    )
  )

(provide 'catkin-test)
