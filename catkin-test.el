(load-file "catkin.el")

(ert-deftest test-catkin-util-format-list-returns-string ()
  "Tests if the catkin-util-format-list function returns a string for given inputs"
  (should (stringp (catkin-util-format-list nil " ")))
  (should (stringp (catkin-util-format-list '("Hello" "World") " ")))
  )

(ert-deftest test-catkin-util-command-to-list-returns-list-of-strings ()
  "Test if the catkin-util-command-to-list function returns a list of strings."
  (let* ((expected-data '("hello" "world"))
         (test-command "printf 'hello\\nworld'")
         (data (catkin-util-command-to-list test-command)))
    (should (sequencep data))
    (should (equal data expected-data))
    )
  )

(ert-deftest test-catkin-util-command-to-list-returns-list-of-strings-with-custom-sep ()
  "Test if the catkin-util-command-to-list function returns a list of strings for a non-default separator."
  (let* ((expected-data '("hello" "world"))
         (test-command "printf 'hello-world'")
         (data (catkin-util-command-to-list test-command "-")))
    (should (sequencep data))
    (should (equal data expected-data))
    )
  )

(provide 'catkin-test)
