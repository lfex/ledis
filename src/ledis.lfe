(defmodule ledis
  (export all))

(defun start-link ()
  (start-link '()))

(defun start-link (options)
  "The 'options' argument may be a combination of ledis options and eredis
  options."
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

(defun set (key value)
  (set key value '()))

(defun set (key value options)
  (parse-result
    (eredis:q (get-client) `("SET" ,key ,value))
    options))

(defun get (key)
  (get key '()))

(defun get (key options)
  (parse-result
    (eredis:q (get-client) `("GET" ,key))
    options))

(defun get-client ()
  (whereis (ledis-cfg:get-client-process-name)))

(defun parse-result
  (((= `#(,status ,data) result) options)
   (case (proplists:get_value 'return-type options (ledis-cfg:get-return-type))
     ('binary result)
     ('string `#(,status ,(binary_to_list data))))))