(defmodule ledis-util
  (export all))

(defun get-version ()
  (lr3-ver-util:get-app-version 'ledis))

(defun get-versions ()
  (++ (lr3-ver-util:get-versions)
      `(#(ledis ,(get-version)))))

(defun make-func-no-options
  ((`(,func-name ,func-arity))
    (let ((func-args (kla:make-args func-arity)))
      `(defun ,func-name ,func-args
         (,func-name ,@func-args '())))))

(defun make-redis-name (func-name)
  (string:tokens
    (string:to_upper
      (atom_to_list func-name))
    "-"))

(defun make-func-options
  ((`(,func-name ,func-arity))
    (let ((func-args (kla:make-args (- func-arity 1))))
      `(defun ,func-name (,@func-args options)
         (cmd (list ,@(make-redis-name func-name) ,@func-args)
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

(defun binlist->strlist (data)
  (lists:map #'binary_to_list/1 data))

(defun make-options (func options)
  (lists:foldl
    (lambda (x accum)
      (++ accum (funcall func x)))
    '()
    options))
