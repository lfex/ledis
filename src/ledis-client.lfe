(defmodule ledis-client
  (export (send 2) (send 3) (send 4)
          (make-client 0) (make-client 2) (make-client 3) (make-client 4))
  (import
    (rename eredis
      ((start_link 4) eredis-start)
      ((q 2) eredis-query)
      ((qp 2) eredis-pipeline))))

(defun format-response (response)
  "Convert binary to list/string."
  (cond ((: erlang is_binary response) (: erlang binary_to_list response))
        ('true response)))

(defun do-query (client command arg)
  "This is a simple wrapper for making a query."
  (let (((tuple 'ok response) (eredis-query client (list command arg))))
    (format-response response)))

(defun do-query (client command arg1 arg2)
  "This is a simple wrapper for making a query."
  (let (((tuple 'ok response) (eredis-query client (list command arg1 arg2))))
    (format-response response)))

(defun make-client ()
  "Create a client and reference it in closures for later use."
  ; > (set client (: ledis make-client))
  ; #Fun<ledis.0.66730582>
  ; > (funcall (funcall client 'get) '"fooz-1")
  ; "barz-1"
  (make-client '"127.0.0.1" 6379 0 '""))

(defun make-client (host)
  (make-client host 6379 0 '""))

(defun make-client (host port)
  (make-client host port 0 '""))

(defun make-client (host port database)
  (make-client host port database '""))

(defun make-client (host port database password)
  (let (((tuple 'ok client) (eredis-start host port database password)))
    (lambda (command)
      (match-lambda
        (('eredis-client) client)
        (((list arg))
          (do-query client command arg))
        (((list arg1 arg2))
          (do-query client command arg1 arg2))))))

(defun get-method (client-maker command)
  ; > (set client (: ledis make-client))
  ; #Fun<ledis.0.66730582>
  ; > (set function (: ledis get-method client 'get))
  ; #Fun<ledis.1.66730582>
  ; > (funcall function '"fooz-1")
  ; "barz-1"
  (funcall client-maker command))

(defun send (client-maker command)
  (funcall (get-method client-maker 'true) command))

(defun send (client-maker command arg)
  ; > (set client (: ledis make-client))
  ; #Fun<ledis.0.131664084>
  ; > (: ledis send client 'get '"fooz-2")
  ; "barz-2"
  (funcall (get-method client-maker command) (list arg)))

(defun send (client-maker command arg1 arg2)
  (funcall (get-method client-maker command) (list arg1 arg2)))