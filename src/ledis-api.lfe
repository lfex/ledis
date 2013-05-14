(defmodule ledis-api
  (export all)
  (import
    (from ledis-client (send 2) (send 3) (send 4))))


; this function is used in the following manner:
;   > (set client (: ledis-client make-client))
;   #Fun<ledis.0.87519432>
;   > (: ledis-api get client '"fooz-4")
;   "barz-4"
(defun get (client-maker key)
  (send client-maker '"GET" key))

(defun set (client-maker key value)
  (send client-maker '"SET" key value))

; this function extracts the native eredis client from the client factory,
; allowing one to execute eredis commands against that client directly, if one
; so chooses
(defun eredis-client (client-maker)
  (send client-maker 'eredis-client))