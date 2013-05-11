(defmodule wrapper1
  (export all)
  (import (from eredis (start_link 0) (q 2) (qp 2))))


; this is the standard way of doing this, using the third party library
(defun do-query ()
  (let (((tuple 'ok client) (start_link)))
    ;(list 1 2 3)))
    (q client '("SET" "fooz-1" "barz-1"))
    (q client '("SET" "fooz-2" "barz-2"))
    (q client '("SET" "fooz-3" "barz-3"))
    (q client '("SET" "fooz-4" "barz-4"))))


; this function is used in the following manner:
;   > (set client (: wrapper1 client-factory))
;   #Fun<wrapper1.0.66730582>
;   > (funcall (funcall client 'get) '"fooz-1")
;   "barz-1"
(defun client-factory ()
  (let (((tuple 'ok client) (start_link)))
  (lambda (message)
      (case message
        ('get (lambda (key)
                (let (((tuple 'ok response) (q client (list '"GET" key))))
                (: erlang binary_to_list response))))))))


; this function is used in the following manner:
;   > (set client (: wrapper1 client-factory))
;   #Fun<wrapper1.0.66730582>
;   > (set function (: wrapper1 get-method client 'get))
;   #Fun<wrapper1.1.66730582>
;   > (funcall function '"fooz-1")
;   "barz-1"
(defun get-method (object message)
  (funcall object message))


; this function is used in the following manner:
;   > (set client (: wrapper1 client-factory))
;   #Fun<wrapper1.0.131664084>
;   > (: wrapper1 send client 'get '"fooz-2")
;   "barz-2"
(defun send (object message key)
  (funcall (get-method object message) key))