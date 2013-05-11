(defmodule wrapper2
  (export all)
  (import (from eredis (start_link 0) (q 2) (qp 2))))


; the code below should be compared to the eredis way of doing things, to assess
; its usability and any gains over eredis in usability. here's eredis:
;   > (set (tuple 'ok client) (: eredis start_link))
;   #(ok <0.31.0>)
;   > (: eredis q client '("GET" "foo2"))
;   #(ok #B(98 97 114 50))
;
; if one imported start_link, q, etc., from eredis in a module, then it would be
; this instead:
;   > (set (tuple 'ok client) (start_link))
;   #(ok <0.31.0>)
;   > (q client '("GET" "foo2"))


; this is a simple wrapper for making a query
(defun do-query (client command arg)
    (let (((tuple 'ok response) (q client (list command arg))))
      (: erlang binary_to_list response)))


(defun do-query (client command arg1 arg2)
    (let (((tuple 'ok response) (q client (list command arg1 arg2))))
      (: erlang binary_to_list response)))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.66730582>
;   > (funcall (funcall client 'get) '"fooz-1")
;   "barz-1"
;
; XXX in this function, we should be able to match lambda for 1, 2, 3, 4, etc.,
; args
(defun client-factory ()
  (let (((tuple 'ok client) (start_link)))
    (lambda (command)
      (match-lambda
        (((list arg))
          (do-query client command arg))
        (((list arg1 arg2))
          (do-query client command arg1 arg2))))))


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


(defun send (object command arg1 arg2)
  (funcall (get-method object command) arg1 arg2))


; this function is used in the following manner:
;   > (set client (: wrapper2 client-factory))
;   #Fun<wrapper2.0.87519432>
;   > (: wrapper2 get client '"fooz-4")
;   "barz-4"
(defun get (client key)
  (send client '"GET" key))


(defun set (client key value)
  (send client '"SET" key value))


; if that last one was to be defined in a ledis module and then imported into
; another project, one would call it in the following manner, assuming you had
; already defined the client:
;   (get client '"fooz-4')


; XXX next, we need to write a macro that lets us pass arbitrary args. this can
; be used for any-arity calls to start_link