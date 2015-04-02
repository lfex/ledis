(defmodule ledis-cfg
  (export all))

(include-lib "lutil/include/core.lfe")

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
    (get-in config 'ledis)))

(defun get-client-process-name ()
  (get-in (ledis-cfg:get-ledis-config) 'client-process-name))

(defun get-return-type ()
  (get-in (ledis-cfg:get-ledis-config) 'return-type))

