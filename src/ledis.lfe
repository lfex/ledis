(defmodule ledis
  (export all))

(defun start-link ()
  (start-link '()))

(defun start-link (options)
  "The 'options' argument may be a combination of ledis options and eredis
  options."
  (logjam:setup)
  (start-link (eredis:start_link options) options))

(defun start-link
  ((`#(ok ,pid) options)
   (register (ledis-cfg:get-client-process-name) pid))
  ((result options)
   result))

(defun start_link ()
  (start-link))

(defun start ()
  (start-link))

(defun cmd (cmd options)
  "This is a general-purpose function that all ledis functions should use when
  making calls to Redis."
  (logjam:debug (MODULE) 'cmd/2 "Command: ~p" `(,cmd))
  (parse-result
    (eredis:q (get-client) cmd)
    options))

(defun set (key value)
  (set key value '()))

(defun set (key value options)
  (let ((cmd (list* "SET" key value (make-set-options options))))
    (cmd cmd options)))

(defun make-set-options (options)
  (lists:foldl
    (lambda (x accum)
      (++ accum (make-set-option x)))
    '()
    options))

(defun make-set-option
  ((`#(ex ,value))
   `("EX" ,value))
  ((`#(px ,value))
   `("PX" ,value))
  ((#(nx))
   '("NX"))
  ((#(xx))
   '("XX"))
  ((_) '()))

(defun get (key)
  (get key '()))

(defun get (key options)
  (cmd `("GET" ,key) options))

(defun incr (key)
  (incr key '()))

(defun incr (key options)
  (cmd `("INCR" ,key) options))

(defun incrby (key value)
  (incrby key value '()))

(defun incrby (key value options)
  (cmd `("INCRBY" ,key ,value) options))

(defun decr (key)
  (decr key '()))

(defun decr (key options)
  (cmd `("DECR" ,key) options))

(defun decrby (key value)
  (decrby key value '()))

(defun decrby (key value options)
  (cmd `("DECRBY" ,key ,value) options))

(defun get-client ()
  (whereis (ledis-cfg:get-client-process-name)))

(defun parse-result
  (((= `#(,status ,data) result) options) (when (is_atom data))
   result)
  (((= `#(,status ,data) result) options)
   (logjam:debug (MODULE) 'parse-result/2 "Result: ~p" `(,result))
   (case (proplists:get_value 'return-type options (ledis-cfg:get-return-type))
     ('binary result)
     ('string `#(,status ,(binary_to_list data))))))
