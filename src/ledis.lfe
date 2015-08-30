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
  (case (get-client)
    ('undefined (start-link))
    (_ #(error already-started))))

(defun start ()
  (start_link))

;;; Generated API functions

(include-lib "ledis/include/ledis.lfe")

;;; Hand-wrought API functions - this is for functions which require special
;;; handling of options, usually in the form of keywords/proplists; simple
;;; positional arguments are handled by the macros which automatically generate
;;; ledis API functions.

(defun bitcount (key options)
  (let ((cmd (list* "BITCOUNT"
                    key
                    (ledis-util:make-options #'make-start-end-option/1 options))))
    (cmd cmd options)))

(defun bitop
  ((operation destkey key options) (when (is_atom key))
   (bitop-single operation destkey key options))
  ((operation destkey key-or-keys options)
   (cond ((io_lib:printable_unicode_list key-or-keys)
          (bitop-single operation destkey key-or-keys options))
         ((is_list key-or-keys)
          (bitop-multi operation destkey key-or-keys options))
         ('true
          (bitop-single operation destkey key-or-keys options)))))

(defun convert-op (op)
  (string:to_upper
    (atom_to_list op)))

(defun bitop-single (operation destkey key options)
  (cmd `("BITOP" ,(convert-op operation) ,destkey ,key) options))

(defun bitop-multi (operation destkey keys options)
  (cmd `("BITOP" ,(convert-op operation) ,destkey ,@keys) options))

(defun bitpos (key bit options)
  (let ((cmd (list* "BITPOS"
                    key
                    bit
                    (ledis-util:make-options #'make-start-end-option/1 options))))
    (cmd cmd options)))

(defun blpop
  ((key timeout options) (when (is_atom key))
   (blpop-single key timeout options))
  ((key-or-keys timeout options)
   (cond ((io_lib:printable_unicode_list key-or-keys)
          (blpop-single key-or-keys timeout options))
         ((is_list key-or-keys)
          (blpop-multi key-or-keys timeout options))
         ('true
          (blpop-single key-or-keys timeout options)))))

(defun blpop-single (key timeout options)
  (cmd `("BLPOP" ,key ,timeout) options))

(defun blpop-multi (keys timeout options)
  (cmd `("BLPOP" ,@keys ,timeout) options))

(defun brpop
  ((key timeout options) (when (is_atom key))
   (brpop-single key timeout options))
  ((key-or-keys timeout options)
   (cond ((io_lib:printable_unicode_list key-or-keys)
          (brpop-single key-or-keys timeout options))
         ((is_list key-or-keys)
          (brpop-multi key-or-keys timeout options))
         ('true
          (brpop-single key-or-keys timeout options)))))

(defun brpop-single (key timeout options)
  (cmd `("BRPOP" ,key ,timeout) options))

(defun brpop-multi (keys timeout options)
  (cmd `("BRPOP" ,@keys ,timeout) options))

(defun client-list (options)
  (case (cmd '("CLIENT" "LIST") options)
    (`#(ok ,clients)
     `#(ok ,(string:tokens clients "\n")))
    (x x)))

(defun del
  ((key options) (when (is_atom key))
   (del-single key options))
  ((key-or-keys options)
   (cond ((io_lib:printable_unicode_list key-or-keys)
          (del-single key-or-keys options))
         ((is_list key-or-keys)
          (del-multi key-or-keys options))
         ('true
          (del-single key-or-keys options)))))

(defun del-single (key options)
  (cmd `("DEL" ,key) options))

(defun del-multi (keys options)
  (cmd `("DEL" ,@keys) options))

(defun lpush
  ((key value options) (when (is_atom value))
   (lpush-single key value options))
  ((key value-or-values options)
   (cond ((io_lib:printable_unicode_list value-or-values)
          (lpush-single key value-or-values options))
         ((is_list value-or-values)
          (lpush-multi key value-or-values options))
         ('true
          (lpush-single key value-or-values options)))))

(defun lpush-single (key value options)
  (cmd `("LPUSH" ,key ,value) options))

(defun lpush-multi (key values options)
  (cmd `("LPUSH" ,key ,@values) options))

(defun lrange (key)
  (lrange key '()))

(defun lrange (key options)
  (lrange key 0 -1 options))

;; The next two functions can't be named like the Redis names (mset and mget),
;; as they would conflict with the LFE functions of the same name.

(defun multi-set (pairs options)
  (cmd `("MSET" ,@pairs) options))

(defun multi-get (keys options)
  (cmd `("MGET" ,@keys) options))

(defun rpush
  ((key value options) (when (is_atom value))
   (rpush-single key value options))
  ((key value-or-values options)
   (cond ((io_lib:printable_unicode_list value-or-values)
          (rpush-single key value-or-values options))
         ((is_list value-or-values)
          (rpush-multi key value-or-values options))
         ('true
          (rpush-single key value-or-values options)))))

(defun rpush-single (key value options)
  (cmd `("RPUSH" ,key ,value) options))

(defun rpush-multi (key values options)
  (cmd `("RPUSH" ,key ,@values) options))

(defun set (key value options)
  (let ((cmd (list* "SET"
                    key
                    value
                    (ledis-util:make-options #'make-set-option/1 options))))
    (cmd cmd options)))

(defun hdel
  ((key field options) (when (is_atom field))
   (hdel-single key field options))
  ((key field-or-fields options)
   (cond ((io_lib:printable_unicode_list field-or-fields)
          (hdel-single key field-or-fields options))
         ((is_list field-or-fields)
          (hdel-multi key field-or-fields options))
         ('true
          (hdel-single key field-or-fields options)))))

(defun hdel-single (key field options)
  (cmd `("HDEL" ,key ,field) options))

(defun hdel-multi (key fields options)
  (cmd `("HDEL" ,key ,@fields) options))

(defun hmget
  ((key field options) (when (is_atom field))
   (hmget-single key field options))
  ((key field-or-fields options)
   (cond ((io_lib:printable_unicode_list field-or-fields)
          (hmget-single key field-or-fields options))
         ((is_list field-or-fields)
          (hmget-multi key field-or-fields options))
         ('true
          (hmget-single key field-or-fields options)))))

(defun hmget-single (key field options)
  (cmd `("HMGET" ,key ,field) options))

(defun hmget-multi (key fields options)
  (cmd `("HMGET" ,key ,@fields) options))

(defun hmset (key pairs)
  (hmset key pairs '()))

(defun hmset (key pairs options)
  (cmd `("HMSET" ,key ,@pairs) options))

;;(defun hmset (key field-value-pairs options)
;;  (cmd `("HMSET" ,key ,@field-value-pairs) options))



;;; General purpose functions for use by API functions

(defun cmd (cmd options)
  "This is a general-purpose function that all ledis functions should use when
  making calls to Redis."
  (logjam:debug (MODULE) 'cmd/2 "Command: ~p" `(,cmd))
  (parse-result
    (eredis:q (get-client) cmd)
    options))

(defun get-client ()
  (whereis (ledis-cfg:get-client-process-name)))

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

(defun make-start-end-option
  ((`#(start ,start))
   `(,start))
  ((`#(end ,end))
   `(,end)))

(defun parse-result
  (((= `#(,status (,data)) result) options) (when (is_atom data))
   `#(,status ,data))
  (((= `#(,status ,data) result) options) (when (is_atom data))
   result)
  (((= `#(,status ,data) result) options) (when (is_list data))
   (logjam:debug (MODULE) 'parse-result/2 "Result: ~p" `(,result))
   (case (proplists:get_value 'return-type options (ledis-cfg:get-return-type))
     ('binary result)
     ('string `#(,status ,(lists:map #'binary_to_list/1 data)))))
  (((= `#(,status ,data) result) options)
   (logjam:debug (MODULE) 'parse-result/2 "Result: ~p" `(,result))
   (case (proplists:get_value 'return-type options (ledis-cfg:get-return-type))
     ('binary result)
     ('string `#(,status ,(binary_to_list data))))))

