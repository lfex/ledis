(defmodule ledis-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'ledis))

(defun get-versions ()
  (++ (lutil:get-versions)
      `(#(ledis ,(get-version)))))

(defun make-func-no-options
  ((`(,func-name ,func-arity))
    (let ((func-args (kla:make-args func-arity)))
      `(defun ,func-name ,func-args
         (,func-name ,@func-args '())))))

(defun make-redis-name (func-name)
  (string:to_upper
    (atom_to_list func-name)))

(defun make-func-options
  ((`(,func-name ,func-arity))
    (let ((func-args (kla:make-args (- func-arity 1))))
      `(defun ,func-name (,@func-args options)
         (cmd (list ,(make-redis-name func-name) ,@func-args)
              options)))))

(defun make-funcs-no-options (func-list)
  (lists:map
    (lambda (x)
      (make-func-no-options x))
    func-list))

(defun make-funcs-options (func-list)
  (lists:map
    (lambda (x)
      (make-func-options x))
    func-list))