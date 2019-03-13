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
    (mock (getenv "EMACS_CATKIN_WS") => npath)
    (should-error (catkin--parse-config nil))
    )
  )

(ert-deftest test-catkin--parse-config-key-with-simple-value ()
  "Test if a keyword with one value like 'null' or 'false' is returned as string"
  (with-mock
    (mock (getenv "EMACS_CATKIN_WS") => path)
    (should (string= "false" (catkin--parse-config "install")))
    )
  )
(ert-deftest test-catkin--parse-config-key-with-list-value ()
  "Test if a keyword with multiple values like 'cmake_args' are returned as list of strings"
  (with-mock
    (mock (getenv "EMACS_CATKIN_WS") => path)
    (should (equal '("-DCMAKE_BUILD_TYPE=Release" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
                   (catkin--parse-config "cmake_args")
                   )
            )
    )
  )
(ert-deftest test-catkin--parse-config--key-with-empty-list-value ()
  "Test if a keyword with an empty list '[]' (yaml syntax) is returned as empty list '() (lisp syntax)"
  (with-mock
    (mock (getenv "EMACS_CATKIN_WS") => path)
    (should-not (catkin--parse-config "blacklist"))
    )
  )

    )
  )

(provide 'catkin-test)
