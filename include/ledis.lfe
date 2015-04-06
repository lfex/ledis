(eval-when-compile

  (defun get-api-funcs-no-opts ()
    '((append 2)
      (auth 1)
      (bgrewriteaof 0)
      (bgsave 0)
      (bitcount 1)
      (bitop 3)
      (bitpos 2)
      (set 2)
      (get 1)
      (getset 2)
      (incr 1)
      (incrby 2)
      (decr 1)
      (decrby 2)
      (multi-set 1)
      (multi-get 1)
      (exists 1)
      (del 1)
      (type 1)
      ))

  (defun get-api-funcs-with-opts ()
    '((append 3)
      (auth 2)
      (bgrewriteaof 1)
      (bgsave 1)
      (get 2)
      (getset 3)
      (incr 2)
      (incrby 3)
      (decr 2)
      (decrby 3)
      (exists 2)
      (type 2)
      ))

  ;; end of eval-when-compile
  )

(defmacro generate-no-opts-api ()
  `(progn ,@(ledis-util:make-funcs-no-options (get-api-funcs-no-opts))))

(defmacro generate-opts-api ()
  `(progn ,@(ledis-util:make-funcs-options (get-api-funcs-with-opts))))

(generate-no-opts-api)
(generate-opts-api)

(defun loaded-ledis ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)