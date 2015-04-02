(defmodule ledis
  (export all))

(defun start-link ()
  (start-link (eredis:start_link)))

(defun start-link
  ((`#(ok ,pid))
   (register (ledis-cfg:client-process) pid))
  ((result)
   result))

(defun start_link ()
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
  (whereis (ledis-cfg:client-process)))

(defun parse-result
  (((= `#(,status ,data) result) options)
   (case (proplists:get_value 'return-type options 'binary)
     ('binary result)
     ('string `#(,status ,(binary_to_list data))))))