(defmodule ledis-api
  (export all)
  (import
    (from ledis-client (send 2) (send 3) (send 4))))

(defun set (client-maker key value)
  "key-value set"
  (send client-maker '"SET" key value))

(defun get (client-maker key)
  "key-value get"
  ; > (set client (: ledis-client make-client))
  ; #Fun<ledis.0.87519432>
  ; > (: ledis-api get client '"fooz-4")
  ; "barz-4"
  (send client-maker '"GET" key))

(defun eredis-client (client-maker)
  "
  This function extracts the native eredis client from the client factory,
  allowing one to execute eredis commands against that client directly, if one
  so chooses.
  "
  (send client-maker 'eredis-client))