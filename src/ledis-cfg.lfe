(defmodule ledis-cfg
  (export all))

(include-lib "clj/include/seq.lfe")

(defun get-ledis-config ()
  (let ((local (get-local-ledis-cfg)))
    (if (=/= local '())
        local
        (get-globgal-ledis-cfg))))

(defun get-local-ledis-cfg ()
  (get-ledis-cfg (lcfg-file:parse-local)))

(defun get-globgal-ledis-cfg ()
  (get-ledis-cfg (lcfg-file:parse-global)))

(defun get-ledis-cfg
  (('())
    '())
  ((config)
    (get-in 'ledis config)))

(defun get-client-process-name ()
  (get-in 'client-process-name (ledis-cfg:get-ledis-config)))

(defun get-return-type ()
  (get-in 'return-type (ledis-cfg:get-ledis-config)))

