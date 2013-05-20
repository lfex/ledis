(defmodule ledis_client_tests
  (export all))

(include-lib "include/lfeunit.lfe")

(defun test_test ()
  "this test generates a bad match error intentionally"
  (let (((tuple 'ok 'true) (list 1 2)))))

(defun assert_test ()
  "is assert available in the namespace?"
  (=:= (+ 1 1) 2))

(defun another_test ()
  "just filler ..."
  'true)