(defmodule ledis-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'ledis))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(ledis ,(get-version)))))