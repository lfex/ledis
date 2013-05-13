(defmodule ledis
  (export all)
  (import
    (rename eredis ((start_link 4) eredis-start)
                   ((q 2) eredis-query)
                   ((qp 2) eredis-pipeline))))

(defun format-response (response)
  (cond ((: erlang is_binary response) (: erlang binary_to_list response))
        ('true response)))

; this is a simple wrapper for making a query
(defun do-query (client command arg)
    (let (((tuple 'ok response) (eredis-query client (list command arg))))
      (format-response response)))

(defun do-query (client command arg1 arg2)
    (let (((tuple 'ok response) (eredis-query client (list command arg1 arg2))))
      (format-response response)))

; this function is used in the following manner:
;   > (set client (: ledis make-client))
;   #Fun<ledis.0.66730582>
;   > (funcall (funcall client 'get) '"fooz-1")
;   "barz-1"
(defun make-client ()
  (make-client '"127.0.0.1" 6379 0 '""))

(defun make-client (host)
  (make-client host 6379 0 '""))

(defun make-client (host port)
  (make-client host port 0 '""))

(defun make-client (host port database)
  (make-client host port database '""))

; XXX in this function, we should be able to match lambda for 1, 2, 3, 4, etc.,
; args; it's broken right now, pending resolution to this issue:
;   https://github.com/oubiwann/ledis/issues/2
(defun make-client (host port database password)
  (let (((tuple 'ok client) (eredis-start host port database password)))
    (lambda (command)
      (match-lambda
        ((arg)
          (do-query client command arg))
        ((arg1 arg2)
          (do-query client command arg1 arg2))))))

; this function is used in the following manner:
;   > (set client (: ledis make-client))
;   #Fun<ledis.0.66730582>
;   > (set function (: ledis get-method client 'get))
;   #Fun<ledis.1.66730582>
;   > (funcall function '"fooz-1")
;   "barz-1"
(defun get-method (client-maker command)
  (funcall client-maker command))

; this function is used in the following manner:
;   > (set client (: ledis make-client))
;   #Fun<ledis.0.131664084>
;   > (: ledis send client 'get '"fooz-2")
;   "barz-2"
(defun send (client-maker command arg)
  (funcall (get-method client-maker command) arg))

(defun send (client-maker command arg1 arg2)
  (funcall (get-method client-maker command) arg1 arg2))

; this function is used in the following manner:
;   > (set client (: ledis make-client))
;   #Fun<ledis.0.87519432>
;   > (: ledis get client '"fooz-4")
;   "barz-4"
(defun get (client-maker key)
  (send client-maker '"GET" key))

(defun set (client-maker key value)
  (send client-maker '"SET" key value))