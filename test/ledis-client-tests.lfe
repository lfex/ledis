(defmodule ledis-client-tests
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest test
  "this test generates a bad match error intentionally"
  (let (((tuple 'ok 'true) (list 1 2)))))

(deftest is
  "is assert available in the namespace?"
  (=:= (+ 1 1) 2))

(deftest another-test
  "just filler ..."
  'true)