; this is the standard way of doing this, using the third party library
(defmodule wrapper1
  (export all)
  (import (from eredis (start_link 0) (q 2) (qp 2))))

(defun do-query ()
  (let (((tuple 'ok client) (start_link)))
    ;(list 1 2 3)))
    (q client '("SET" "fooz-1" "barz-1"))
    (q client '("SET" "fooz-2" "barz-2"))
    (q client '("SET" "fooz-3" "barz-3"))
    (q client '("SET" "fooz-4" "barz-4"))))