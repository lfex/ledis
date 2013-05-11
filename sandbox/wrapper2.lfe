(defmodule wrapper2
  (export all)
  (import (from eredis (start_link 0) (q 2) (qp 2))))


; this is a simple wrapper for making a query
(defun do-query (client command arg)
  (let (((tuple 'ok response) (q client (list command arg))))
    (: erlang binary_to_list response)))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.66730582>
;   > (funcall (funcall client 'get) '"fooz-1")
;   "barz-1"
(defun client-factory ()
  (let (((tuple 'ok client) (start_link)))
  (lambda (command)
    (lambda (arg)
      (do-query client command arg)))))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.66730582>
;   > (set function (: wrapper2 get-method client 'get))
;   #Fun<wrapper2.1.66730582>
;   > (funcall function '"fooz-1")
;   "barz-1"
(defun get-method (object command)
  (funcall object command))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.131664084>
;   > (: wrapper2 send client 'get '"fooz-2")
;   "barz-2"
(defun send (object command arg)
  (funcall (get-method object command) arg))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.87519432>
;   > (: wrapper2 get client '"fooz-4")
;   "barz-4"
(defun get (client arg)
  (send client '"GET" arg))